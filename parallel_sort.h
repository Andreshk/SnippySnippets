#pragma once
#include <algorithm>    // std::sort, std::inplace_merge
#include <vector>
#include <thread>
#include <atomic>
#include <iterator>     // std::iterator_traits
#include <type_traits>  // std::enable_if, std::is_same_v

// Uncomment for detailed thread interaction info
//#define PARALLEL_SORT_DEBUG

#ifdef PARALLEL_SORT_DEBUG
#include <iostream>
#include <mutex>
std::mutex coutmtx;
#endif // PARALLEL_SORT_DEBUG

// Really looking forward to Concepts...
template <class RanIt>
inline constexpr bool is_random_iterator = std::is_same_v<typename std::iterator_traits<RanIt>::iterator_category, std::random_access_iterator_tag>;

// Forward declaration
template<class RanIt, class Pred>
class impl;

// Parallel sort: given n threads, split the array to be sorted in n equal subarrays,
// sort the subarrays concurrently, and then perform pairwise merging, also in parallel (!)
template<class RanIt>
void parallel_sort(RanIt from, RanIt to, size_t nthreads = std::thread::hardware_concurrency()) {
    // Use std::less<typename std::iterator_traits<RanIt>::value_type>, if C++14 is not available
    parallel_sort(from, to, nthreads, std::less<>{});
}

// The comparison predicate should be thread safe in the sense that different threads
// will call Pred::operator() on different copies of the predicate at the same time.
template <class RanIt, class Pred>
std::enable_if_t<is_random_iterator<RanIt>>
    parallel_sort(RanIt from, RanIt to, size_t nthreads = std::thread::hardware_concurrency(), Pred pr = Pred{})
{
    // Clamp nthreads to a valid value
    if (nthreads == 0)
        nthreads = 1;
    const size_t n = size_t(to - from); // Not a narrowing conversion actually
    if (nthreads > n)
        nthreads = n;
    // Create an 'impl' instance, which keeps the internal sorting state
    // (thread pool, counters, subarray limits, etc.) and does all the work.
    impl<RanIt, Pred> sorter{ from,to,nthreads,pr };
}

// Encapsulates everything needed for parallel sorting and merging:
// thread array, per-thread subarray info, number of threads active, etc.
template<class RanIt, class Pred>
class impl {
    friend void parallel_sort<RanIt, Pred>(RanIt, RanIt, size_t, Pred);

    std::vector<std::thread> ths;    // thread array
    const size_t nthreads;           // total thread count
    const size_t nthrpow2;           // smallest power of 2, >= nthreads
    std::atomic<size_t> ths_created; // number of threads created at a given moment
#ifdef PARALLEL_SORT_DEBUG
    std::atomic<size_t> ths_joined;  // number of threads, joined after completing their work
#endif // PARALLEL_SORT_DEBUG

    std::vector<RanIt> froms; // per-thread subarray beginning
    std::vector<RanIt> tos;   // per-thread subarray end
    Pred pr; // comparison predicate

    static size_t smallestPowOf2(const size_t n) {
        size_t res = 1;
        while (res < n) res *= 2;
        return res;
    }

    impl(RanIt from, RanIt to, size_t _nthreads, Pred _pr)
        : ths{}, nthreads{ _nthreads }, nthrpow2{ smallestPowOf2(_nthreads) }
        , ths_created{ 0 }, froms{ _nthreads }, tos{ _nthreads } , pr{ _pr }
#ifdef PARALLEL_SORT_DEBUG
        , ths_joined{ 0 }
#endif // PARALLEL_SORT_DEBUG
    {
        // Reserve memory to guarantee that no vector reallocations will occur
        // - absolutely necessary for the interactions between threads to work properly (!)
        ths.reserve(nthreads);
        const size_t subSize = (to - from) / nthreads;
        for (size_t idx = 0; idx < nthreads; ++idx) {
            // Calculate the subarrays each thread has to sort
            froms[idx] = from + subSize*idx;
            tos[idx] = froms[idx] + subSize;
        }
        // The final subarray may be a little bit bigger (no more than nthreads more elements than the rest)
        tos[nthreads - 1] = to;
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
        std::cout << "nthreads=" << nthreads << "\nnthrpow2=" << nthrpow2
                  << "\nths_created=" << ths_created << "\nths_joined=" << ths_joined << "\n";
#endif // PARALLEL_SORT_DEBUG
    }

    // Per-thread work: sort each subarray naively, then parallel merge
    void sort(const size_t idx) {
        std::sort(froms[idx], tos[idx], pr);
        merge_threads(idx);
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
    void merge_threads(const size_t idx) {
        for (size_t i = 1; i < nthrpow2; i *= 2)
            if (idx & i) {
                return; // wait to die
            } else {
                // Find the victim (thread to merge)...
                size_t idx_to_kill = idx + i;
                // Nobody left to kill, so wait to die as well
                if (idx_to_kill >= nthreads)
                    return;
                // Make sure the victim has actually been created...
                // (needed when per-thread work takes less time than thread creation)
                while (idx_to_kill >= ths_created) {}
                // ...then kill it (after it's done its job)
                ths[idx_to_kill].join();
#ifdef PARALLEL_SORT_DEBUG
                ++ths_joined;
                coutmtx.lock();
                std::cout << "#" << idx << " <- #" << idx_to_kill << "\n";
                coutmtx.unlock();
#endif // PARALLEL_SORT_DEBUG
                // This thread's subarray and the victim's subarray are actually in consecutive memory.
                std::inplace_merge(froms[idx], froms[idx_to_kill], tos[idx_to_kill], pr);
                // Some false cache sharing will occur here, but no need for
                // atomic operations - no two threads write to a same location,
                // and the single read is after all writes to this location.
                tos[idx] = tos[idx_to_kill];
            }
    }
};
