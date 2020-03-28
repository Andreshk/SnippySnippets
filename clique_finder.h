#pragma once
#include <cstdint> // uint64_t, uint16_t
#include <vector>
#include <unordered_map>

#include <iostream>    // Debugging & testing purposes only
#include <chrono>      // Debugging & testing purposes only
#include <string_view> // Debugging & testing purposes only

#include <cassert>
#define vassert assert

// Used for optimal detection of cliques, i.e. find a way to group the connected vertices
// into as few cliques as possible. Since the core algorithm is recursive, this class
// maintains the state needed at each step of the computation.
// The algorithm is limited to 16 vertices.
class CliqueFinder {
    // A partitioning of vertices into cliques represents a compressed vertex-to-clique map.
    // Since there are at most 16 vertices, there can be at most 16 cliques, so each clique
    // index fits in 4 bits, and 16 clique indices fit in a single uint64_t.
    // The indices are in consecutive 4-bit chunks, starting from the lowest bits for vertex 0.
    // Important: a partition does not carry information from how many vertices it was built,
    // nor how many cliques are currently formed. This is done as an optimization, since Partition-s
    // are used as hashmap keys.
    class Partition {
        uint64_t bits;

        // Equivalent partitions, such as {0,0,1,1} and {1,1,0,0} should be represented as identical
        // integers in order for the hashing to work. So, we enforce an ordering in the indices of
        // a partition - the first occurrences of every index should be in increasing order.
        // This means {0,0,1,1} and {1,1,0,0} will be both represented by {0,0,1,1}.
        // @param[in] numVertices the number of vertices, for which this partition is built
        bool isValid(const int numVertices) const {
            int maxIdx = 0;
            // Loop through the clique indices - each should be at most 1+ the maximum index before it.
            for (int v = 0; v < numVertices; ++v) {
                if (getCliqueIndex(v) == maxIdx + 1) {
                    ++maxIdx; // Update the maximum
                } else if (getCliqueIndex(v) > maxIdx) {
                    return false;
                }
            }
            return true;
        }
        // Sets the clique index for a given vertex. Does not clear the corresponding bits beforehand.
        // @param[in] v Vertex index
        // @param[in] val Clique index for this vertex.
        void set(const int v, const int val) {
            vassert(v >= 0 && v < 16 && val >= 0 && val < 16);
            vassert(getCliqueIndex(v) == 0);
            bits |= (uint64_t(val) << (4 * v));
        }
    public:
        // Creates a "trivial" partition - each vertex is in its own clique only
        // @param[in] numVertices The total number of vertices. Determines the number of unused bits.
        Partition(const int numVertices) {
            bits = 0;
            for (int v = 0; v < numVertices; ++v) {
                set(v, v);
            }
            vassert(isValid(numVertices));
        }
        // Creates a new partition by merging two cliques from another partition
        // @param[in] other The partition, whose cliques are merges
        // @param[in] i Index of the clique, to which the other will be merged
        // @param[in] j Index of the clique, which will be merged into the other
        // @param[in] numVertices The number of devices, from which 'other' has been built.
        Partition(const Partition &other, const int i, const int j, const int numVertices) {
            vassert(i < j); // For maintaining the class invariant (see isValid()).
            bits = 0;
            // By merging cliques i and j, all clique indices after j should be decremented to account for the lowered clique count.
            for (int v = 0; v < numVertices; ++v) {
                const int otherIdxForV = other.getCliqueIndex(v);
                if (otherIdxForV < j) {
                    set(v, otherIdxForV);
                } else if (otherIdxForV == j) {
                    set(v, i);
                } else {
                    set(v, otherIdxForV - 1);
                }
            }
            vassert(isValid(numVertices));
        }
        // Equivalence check, required for hashing
        friend bool operator==(const Partition &p1, const Partition &p2) {
            return (p1.bits == p2.bits);
        }
        // Get the clique index for a given vertex.
        // @param[in] i Vertex index. Not checked whether <numVertices.
        int getCliqueIndex(const int i) const {
            vassert(i >= 0 && i < 16);
            return ((bits >> (4 * i)) & 0xF);
        }

        // A partition is hashed by simply hashing its bitmask
        struct Hasher {
            std::size_t operator()(const Partition& p) const {
                return std::hash<uint64_t>()(p.bits);
            }
        };
    };

    // A pair of a Partition with the number of islands it's currently split into.
    struct SizedPartition {
        Partition partition;
        int numCliques;
    };

    // The graph, represented as an adjacency matrix. The value at index i is a bitmask,
    // whose bit j (starting from lowest) is set when there's a connection from vertex i to vertex j.
    const std::vector<uint16_t>& neighbs;
    // Mapping from a given partition to the optimal partition, obtained from joining 2 or more of its cliques
    std::unordered_map<Partition, SizedPartition, Partition::Hasher> cache;
    // The number of vertices. Cached here once, so that Partition-s themselves do not keep a copy of it.
    const int numVertices;
    // Whether the optimal clique partitioning is trivial, i.e. every vertex is in its own clique.
    const bool isTrivial;

    // Find out whether the given graph has a trivial optimal partitioning
    static bool checkIsTrivial(const std::vector<uint16_t>& neighbs) {
        for (const auto& nbs : neighbs) {
            // We do not expect a vertex to have an explicit edge to itself
            if (nbs != 0) {
                return false;
            }
        }
        return true;
    }

    // Check whether two cliques of a partition can be joined - i.e. there is
    // an edge from every vertex in clique i to every vertex in clique j.
    bool joinable(const Partition p, const int i, const int j) const {
        for (int u = 0; u < numVertices; ++u) {
            // Filter every u, whose island is i
            if (p.getCliqueIndex(u) != i) {
                continue;
            }
            for (int v = 0; v < numVertices; ++v) {
                // Filter every v, whose island is j
                if (p.getCliqueIndex(v) != j) {
                    continue;
                }
                // If there is no NVLink between u and v, their cliques are not joinable
                if ((neighbs[u] & (uint16_t(1) << v)) == 0 || (neighbs[v] & (uint16_t(1) << u)) == 0) {
                    return false;
                }
            }
        }
        return true;
    }

    using CacheIterator = decltype(cache)::const_iterator;
    CacheIterator findOptimalMerge(const SizedPartition sp) {
        // This is an optimal partition - all devices are in the same cliques, no attempts to optimize it should be made.
        if (sp.numCliques == 1) {
            return cache.insert(std::make_pair(sp.partition, sp)).first;
        }
        // For each pair of cliques in the current partition that can be merged,
        // merge them and continue (recursively) attempting to merge even more.
        // At each step, update the best partition, obtained from the current one.
        // Of course, in the beginning the optimal partition _is_ the current one.
        SizedPartition best = sp;
        for (int i = 0; i < sp.numCliques; ++i) {
            for (int j = i + 1; j < sp.numCliques; ++j) {
                // Attempt to join cliques i and j as a first step to the optimal partition
                if (joinable(sp.partition, i, j)) {
                    Partition joined{ sp.partition, i, j, numVertices };
                    CacheIterator it = cache.find(joined);
                    // If there is no result cached for the 1-step joined partition, make the full recursive computation.
                    if (it == cache.end()) {
                        it = findOptimalMerge(SizedPartition{ joined, sp.numCliques - 1 });
                    }
                    const SizedPartition &res = it->second;
                    // Compare the result of this 1-step merge with the best result so far
                    if (res.numCliques < best.numCliques) {
                        best = res;
                    }
                    // There can be no better partition -> break all searches & exit as fast as possible
                    // to the top recursive call, without even updating the cache.
                    if (best.numCliques == 1) {
                        return it;
                    }
                }
            }
        }
        // We have found the best partition, obtained from the current one -> update the cache and return the result.
        return cache.insert(std::make_pair(sp.partition, best)).first;
    }
public:
    CliqueFinder(const std::vector<uint16_t>& neighbs)
        : neighbs{ neighbs }
        , numVertices{ int(neighbs.size()) }
        , isTrivial{ (neighbs.size() > 16 || checkIsTrivial(neighbs)) }
    {
        vassert(numVertices <= 16);
    }

    std::vector<std::vector<int>> findCliques() {
        if (isTrivial) {
            std::vector<std::vector<int>> cliques(numVertices);
            for (int v = 0; v < numVertices; ++v) {
                cliques[v].push_back(v);
            }
            return cliques;
        }
        vassert(cache.empty());
        const auto it = findOptimalMerge(SizedPartition{ Partition{ numVertices }, numVertices });
        const SizedPartition& sp = it->second;
        std::vector<std::vector<int>> cliques(sp.numCliques);
        for (int v = 0; v < numVertices; ++v) {
            cliques[sp.partition.getCliqueIndex(v)].push_back(v);
        }
        cache.clear(); // Not the this invalidated the iterator it
        return cliques;
    }

    // 
    // For testing purposes: create several types of commonly-found graphs
    // 

    // In a ring-graph there is a single loop, passing through all vertices (Google Ring)
    static std::vector<uint16_t> makeRingGraph(const int numVertices) {
        vassert(numVertices >= 0 && numVertices <= 16);
        std::vector<uint16_t> res(numVertices, 0);
        for (int v = 0; v < numVertices; ++v) {
            res[v] |= (1ui16 << ((v + numVertices - 1) % numVertices));
            res[v] |= (1ui16 << ((v + 1) % numVertices));
        }
        return res;
    }

    // In a complete graph, every vertex has an edge to every other vertex.
    static std::vector<uint16_t> makeCompleteGraph(const int numVertices) {
        vassert(numVertices >= 0 && numVertices <= 16);
        std::vector<uint16_t> res(numVertices, 0);
        for (int v = 0; v < numVertices; ++v) {
            res[v] = ~(1ui16 << v); // No point in having an edge to itself
        }
        return res;
    }

    // Make a graph for the biggest hypercube with size <=numVertices
    static std::vector<uint16_t> makeHypercube(const int numVertices) {
        vassert(numVertices >= 0 && numVertices <= 16);
        std::vector<uint16_t> res(numVertices, 0);
        const int degree = [](int n) { int deg = 0; while (n >>= 1) { ++deg; }; return deg; }(numVertices);
        // Recursively build the hypercube by connecting two copies of the hypercube with a lower degree
        for (int pass = 0; pass < degree; ++pass) {
            // The number of vertices in the lower-degree hypercube
            const int oldn = (1 << pass);
            // Mask off the lower oldn bits
            const uint16_t mask = (1ui16 << oldn) - 1;
            for (int d = 0; d < oldn; ++d) {
                // First - for each vertex of the old hypercube, add an edge to its copy in the new hypercube
                res[d] |= (1ui16 << (d + oldn));
                // Each vertex of the second copy will have similar connections, but mirrored
                // (i.e. the vertex indices will be after the indices of the first, so we shift left the older copy's bitmask)
                // We also have to copy (and mirror) the connection from each old vertex to its new copy
                const uint16_t lower = res[d];
                res[d + oldn] = (((lower >> oldn) & mask) | ((lower & mask) << oldn));
            }
        }
        return res;
    }

    static void testClique(const std::string_view msg, const std::vector<uint16_t>& graph) {
        CliqueFinder cf{ graph };
        using Clock = std::chrono::steady_clock;
        const auto start = Clock::now();
        const auto cliques = cf.findCliques();
        const auto end = Clock::now();
        const long long us = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
        const double ms = double(us) / 1000;
        std::cout << msg << "\nnumCliques = " << cliques.size() << " (" << ms << "ms)";
        for (const auto cl : cliques) {
            std::cout << "\n ";
            for (const auto d : cl) {
                std::cout << ' ' << d;
            }
        }
        std::cout << '\n';
    }

    static void runTests() {
        testClique("Simple 2-device setup",
            { 0b10, 0b01 });
        testClique("Google ring, 8 devices",
            makeRingGraph(8));
        testClique("Google ring, 16 devices",
            makeRingGraph(16));
        testClique("DGX-1",
            { 0b00011110, 0b00101101, 0b01001011, 0b10000111,
              0b11100001, 0b11010010, 0b10110100, 0b01111000 });
        testClique("NVLink fabric, 8 devices",
            makeCompleteGraph(8));
        testClique("NVLink fabric, 16 devices",
            makeCompleteGraph(16));
        testClique("Hypercube, 2 devices",
            { 0b10, 0b01 });
        testClique("Hypercube, 8 devices",
            makeHypercube(8));
        testClique("Hypercube, 16 devices",
            makeHypercube(16));
    }
};

#undef vassert
