#pragma once
#include <iostream>
#include <mutex>
#include <set>

#ifndef vassert
#include <cassert>
#define vassert assert
#endif // vassert

class NodeCounter {
    std::set<void*> nodes = {};
    size_t numAllocated   = 0;
    size_t numDeallocated = 0;
    std::mutex mtx;

    static NodeCounter counter;
    NodeCounter() = default;
public:
    // These two should be used only from PersistentVector<T>::Node::Node() and release()
    static void add(void* ptr) {
        //std::cout << "Constructing 0x" << ptr << '\n';
        std::lock_guard lock{ counter.mtx };
        vassert(counter.nodes.find(ptr) == counter.nodes.end());
        counter.nodes.insert(ptr);
        ++counter.numAllocated;
    }
    static void remove(void* ptr) {
        //std::cout << "Destroying   0x" << ptr << '\n';
        std::lock_guard lock{ counter.mtx };
        const auto it = counter.nodes.find(ptr);
        vassert(it != counter.nodes.end());
        counter.nodes.erase(it);
        ++counter.numDeallocated;
    }
    // Output the current allocation statistics
    static void print() {
        std::lock_guard lock{ counter.mtx };
        std::cout << "  Total allocations:   " << counter.numAllocated   << '\n';
        std::cout << "  Total deallocations: " << counter.numDeallocated << '\n';
        std::cout << "  Currently allocated: " << counter.nodes.size() << "\n\n";
    }
    ~NodeCounter() {
        vassert(counter.nodes.size() == 0 && "Memory leak - not all nodes have been deallocated!");
        std::cout << "NodeCounter stats on destruction:\n";
        print();
    }
};
// inline implies external linkage since C++17 => safe to define in this header
inline NodeCounter NodeCounter::counter{};
#define DEBUG_ONLY(expr) expr
