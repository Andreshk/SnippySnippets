#pragma once
#include <vector>
#include <thread>
#include <atomic>
#include <iterator>    // std::random_access_iterator
#include <algorithm>   // std::ranges::sort, std::ranges::merge
#include <type_traits> // std::enable_if, std::is_same_v

#ifndef vassert
#include <cassert>
#define vassert assert
#endif

// Uncomment for detailed thread interaction info
//#define PARALLEL_SORT_DEBUG

#ifdef PARALLEL_SORT_DEBUG
#include <iostream>
#include <mutex>
std::mutex coutmtx;
#endif // PARALLEL_SORT_DEBUG

// Forward declaration
template<std::ranges::random_access_range Range, class Pred>
class impl;

// Parallel sort: given n threads, split the array to be sorted in n equal subarrays,
// sort the subarrays concurrently, and then perform pairwise merging, also in parallel (!)
// The comparison predicate should be thread safe in the sense that different threads
// will call Pred::operator() on different copies of the predicate at the same time.
template <std::ranges::random_access_range Range, class Pred = std::ranges::less>
    requires std::sortable<std::ranges::iterator_t<Range>, Pred>
void parallel_sort(Range& xs, size_t nthreads = std::thread::hardware_concurrency(), Pred pr = Pred{}) {
    // Clamp nthreads to a valid value
    if (nthreads == 0) {
        nthreads = 1;
    }
    const size_t n = std::ranges::size(xs);
    if (nthreads > n) {
        nthreads = n;
    }
    // Create an 'impl' instance, which keeps the internal sorting state
    // (thread pool, counters, subarray limits, etc.) and does all the work.
    impl<Range, Pred> sorter{ xs,nthreads,pr };
}

// Encapsulates everything needed for parallel sorting and merging:
// thread array, per-thread subarray info, number of threads active, etc.
template<std::ranges::random_access_range Range, class Pred>
class impl {
    friend void parallel_sort<Range, Pred>(Range&, size_t, Pred);
    using Iter = std::ranges::iterator_t<Range>;
    using diff_t = typename Iter::difference_type;

    // Scratch memory, to be used during merging
    std::vector<typename Iter::value_type> scratch;
    std::vector<std::thread> ths;    // thread array
    const size_t nthreads;           // total thread count
    const size_t numSteps;           // number of steps for the parallel merging (<=> logarithm of smallest power of 2, >= nthreads)
    std::atomic<size_t> ths_created; // number of threads created at a given moment
#ifdef PARALLEL_SORT_DEBUG
    std::atomic<size_t> ths_joined;  // number of threads, joined after completing their work
#endif // PARALLEL_SORT_DEBUG
    const Iter from;
    std::vector<diff_t> froms; // per-thread subarray beginning
    std::vector<diff_t> tos;   // per-thread subarray end
    Pred pr; // comparison predicate

    static size_t logPowOf2(const size_t n) {
        size_t res = 0;
        while ((1ui64 << res) < n) { ++res; }
        return res;
    }

    impl(Range& xs, size_t _nthreads, Pred _pr)
        : scratch(std::ranges::size(xs)), ths{}, nthreads{ _nthreads }, numSteps{ logPowOf2(_nthreads) }
        , ths_created{ 0 }, from{ std::ranges::begin(xs) }, froms(_nthreads), tos(_nthreads), pr{ _pr }
#ifdef PARALLEL_SORT_DEBUG
        , ths_joined{ 0 }
#endif // PARALLEL_SORT_DEBUG
    {
        // Reserve memory to guarantee that no vector reallocations will occur
        // - absolutely necessary for the interactions between threads to work properly (!)
        ths.reserve(nthreads);
        const size_t n = std::ranges::size(xs);
        const size_t subSize = n / nthreads;
        for (size_t idx = 0; idx < nthreads; ++idx) {
            // Calculate the subarrays each thread has to sort
            froms[idx] = subSize*idx;
            tos[idx] = froms[idx] + subSize;
        }
        // The final subarray may be a little bit bigger (no more than nthreads more elements than the rest)
        tos[nthreads - 1] = n;
        // Invariant: at _any discrete moment of time_ 'ths_created' is
        // not greater than the # of actually constructed std::threads.
        // This is relied on by the busy-waiting loop in merge_threads().
        for (size_t idx = 0; idx < nthreads; ++idx) {
            ths.emplace_back(&impl::sort, this, idx);
            ++ths_created;
        }
        // All other worker threads will be taken care of by ths[0] before
        // it finishes, so the main thread blocks until it completes.
        ths[0].join();
#ifdef PARALLEL_SORT_DEBUG
        ++ths_joined;
        vassert(ths_created == ths_joined);
        std::cout << "nthreads=" << nthreads << "\nnumSteps=" << numSteps
                  << "\nths_created=" << ths_created << "\nths_joined=" << ths_joined << "\n";
#endif // PARALLEL_SORT_DEBUG
        if (numSteps % 2 == 1) {
            std::ranges::copy(scratch.cbegin(), scratch.cend(), from);
        }
    }

    // Per-thread work: sort each subarray naively, then parallel merge
    void sort(const size_t threadIdx) {
        std::ranges::sort(from + froms[threadIdx], from + tos[threadIdx], pr);
        merge_threads(threadIdx);
    }

    /* tl;dr: Parallel merge
    * Suppose we have 8 worker threads. After all per-thread work is done, the
    * threads (and the subarrays) are merged pairwise _in parallel_ until there
    * is only one thread left. Here i,j merging means thread #i merges the
    * subarray of thread #j into its own and kills thread #j, denoted i<-j.
    * 0<-1 2<-3 4<-5 6<-7 (in parallel), then
    * 0 <- 2    4 <- 6, then
    * 0    <-   4
    * and afterwards only worker thread #0 will be alive and have the entire array sorted.
    */
    void merge_threads(const size_t threadIdx) {
        for (size_t step = 0; step < numSteps; ++step) {
            const int mask = (1ui64 << step);
            if (threadIdx & mask) {
                return; // wait to die
            } else {
                // Find the victim (thread to merge)...
                size_t idx_to_kill = threadIdx + mask;
                // Nobody left to kill, so wait to die as well
                if (idx_to_kill >= nthreads) {
                    return;
                }
                // Make sure the victim has actually been created...
                // (needed when per-thread work takes less time than thread creation)
                while (idx_to_kill >= ths_created) {}
                // ...then kill it (after it's done its job)
                ths[idx_to_kill].join();
#ifdef PARALLEL_SORT_DEBUG
                ++ths_joined;
                coutmtx.lock();
                std::cout << "#" << threadIdx << " <- #" << idx_to_kill << "\n";
                coutmtx.unlock();
#endif // PARALLEL_SORT_DEBUG
                vassert(tos[threadIdx] == froms[idx_to_kill]);
                if (step % 2 == 0) {
                    const Iter start = from + froms[threadIdx];
                    const Iter mid = from + froms[idx_to_kill];
                    const Iter end = from + tos[idx_to_kill];
                    // This thread's subarray and the victim's subarray are actually in consecutive memory.
                    std::ranges::merge(start, mid, mid, end, scratch.begin() + froms[threadIdx], pr);
                } else {
                    const Iter start = scratch.begin() + froms[threadIdx];
                    const Iter mid = scratch.begin() + froms[idx_to_kill];
                    const Iter end = scratch.begin() + tos[idx_to_kill];
                    std::ranges::merge(start, mid, mid, end, from + froms[threadIdx], pr);
                }
                // Some false cache sharing will occur here, but no need for
                // atomic operations - no two threads write to a same location,
                // and the single read is after all writes to this location.
                tos[threadIdx] = tos[idx_to_kill];
            }
        }
    }
};

#undef vassert
