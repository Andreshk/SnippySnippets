#pragma once
#include <vector>
#include <thread>
#include <atomic>
#include <ranges>    // std::random_access_range
#include <algorithm> // std::ranges::sort, std::ranges::merge

#ifndef vassert
#include <cassert>
#define vassert assert
#endif

// Uncomment for detailed thread interaction info
//#define PARALLEL_SORT_DEBUG

#ifdef PARALLEL_SORT_DEBUG
#include <iostream>
#include <mutex>
#endif // PARALLEL_SORT_DEBUG

// Calculates ceil(log2(n)). This will be the number of merging steps.
size_t logPowOf2(const size_t n) {
    size_t res = 0;
    while ((1ui64 << res) < n) { ++res; }
    return res;
}

// Parallel sort: given n threads, split the array to be sorted in n equal subarrays,
// sort the subarrays concurrently, and then perform pairwise merging, also in parallel (!)
// The comparison predicate's operator() should be thread safe.
template <std::ranges::random_access_range Range, class Pred = std::ranges::less>
    requires std::sortable<std::ranges::iterator_t<Range>, Pred>
void parallel_sort(Range& xs, size_t numThreads = std::thread::hardware_concurrency(), Pred pr = Pred{}) {
    // Clamp numThreads to a valid value
    if (numThreads == 0) {
        numThreads = 1;
    }
    const size_t n = std::ranges::size(xs);
    if (numThreads > n) {
        numThreads = n;
    }

    using Iter = std::ranges::iterator_t<Range>;
    // Scratch memory, to be used during merging (also for double buffering)
    std::vector<typename Iter::value_type> scratch(n);
    // Number of steps for the parallel, pair-wise merging
    const size_t numSteps = logPowOf2(numThreads);
    // Number of threads created at a given moment
    std::atomic<size_t> numCreated = 0;
    const Iter xsBegin = std::ranges::begin(xs);
    const auto scrBegin = std::ranges::begin(scratch);
    using diff_t = typename Iter::difference_type;
    // For each thread with index i, the subarray to sort will be [offsets[i];offsets[i+1])
    // An immediately-invoked lambda is used just to make the result const.
    const std::vector<diff_t> offsets = [&]() {
        std::vector<diff_t> res(numThreads + 1);
        const size_t subSize = n / numThreads;
        for (size_t idx = 0; idx < numThreads; ++idx) {
            res[idx] = subSize * idx;
        }
        // The final subarray may be a little bit bigger (no more than numThreads more elements than the rest)
        res.back() = n;
        return res;
    }();
    // Thread array
    std::vector<std::thread> ths;

    // Merges a subarray of 'from' into the corrpespoding subarray of 'to'.
    // Will be used between the main array and the scratch buffer and vice versa.
    auto singleMerge = [&](const auto& from, auto& to, const size_t threadIdx, const size_t idxToKill) {
        const auto begin = std::ranges::cbegin(from);
        const auto start = begin + offsets[threadIdx];
        const auto mid = begin + offsets[idxToKill];
        const size_t endIdx = std::min(idxToKill + (idxToKill - threadIdx), numThreads);
        const auto end = begin + offsets[endIdx];
        const auto res = std::ranges::begin(to) + offsets[threadIdx];
        // This thread's subarray and the victim's subarray are actually in consecutive memory.
        std::ranges::merge(start, mid, mid, end, res, pr);
    };

    // Per-thread work: sort each subarray naively, then parallel merge
    auto work = [&](const size_t threadIdx) {
        std::ranges::sort(xsBegin + offsets[threadIdx], xsBegin + offsets[threadIdx + 1], pr);
        /* Suppose we have 8 worker threads. After per-thread sorting is done, the
         * threads (and the subarrays) are merged pairwise _in parallel_ until there
         * is only one thread left. Here i,j merging means thread #i merges the
         * subarray of thread #j into its own and kills thread #j, denoted i<-j.
         * The merging steps will then be:
         * 0<-1 2<-3 4<-5 6<-7, then
         * 0 <- 2    4 <- 6, then
         * 0    <-   4
         * and afterwards only worker thread #0 will be alive and have the entire array sorted.
         */
        for (size_t step = 0; step < numSteps; ++step) {
            const size_t mask = (1ui64 << step);
            if (threadIdx & mask) {
                return; // wait to die
            } else {
                // Find the victim (thread to merge)...
                const size_t idxToKill = threadIdx + mask;
                // Nobody left to kill, so wait to die as well
                if (idxToKill >= numThreads) {
                    return;
                }
                // Make sure the victim has actually been created...
                // (needed when per-thread work takes less time than thread creation)
                while (idxToKill >= numCreated) {}
                // ...then kill it after it's done its job.
                ths[idxToKill].join();
#ifdef PARALLEL_SORT_DEBUG
                static std::mutex coutmtx;
                {
                    std::lock_guard lock{ coutmtx };
                    std::cout << "#" << threadIdx << " <- #" << idxToKill << "\n";
                }
#endif // PARALLEL_SORT_DEBUG
                if (step % 2 == 0) {
                    singleMerge(xs, scratch, threadIdx, idxToKill);
                } else {
                    singleMerge(scratch, xs, threadIdx, idxToKill);
                }
            }
        }
    };

    // Reserve memory to guarantee that no vector reallocations will occur
    // - absolutely necessary, to avoid race conditions on the vector itself (!)
    ths.reserve(numThreads);
    // Invariant: at _any discrete moment of time_ 'numCreated' is
    // not greater than the # of actually constructed std::threads.
    // This is relied on by the busy-waiting loop in work().
    for (size_t threadIdx = 0; threadIdx < numThreads; ++threadIdx) {
        ths.emplace_back(work, threadIdx);
        ++numCreated;
    }
    // All other worker threads will be taken care of by ths[0] before
    // it finishes, so the main thread blocks until it completes.
    ths[0].join();
#ifdef PARALLEL_SORT_DEBUG
    std::cout << "main <- #0\n"
              << "numThreads=" << numThreads
              << "\nnumSteps=" << numSteps
              << "\nn=" << n << "\n";
#endif // PARALLEL_SORT_DEBUG
    // TO-DO: This should be multithreaded, too (!)
    // Threads that die should instead wait on a conditional
    // variable to be woken up again for this step.
    if (numSteps % 2 == 1) {
        std::ranges::copy(scratch.cbegin(), scratch.cend(), xsBegin);
    }
}

#undef vassert
