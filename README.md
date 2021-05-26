# SnippySnippets
A repository for random, useful or interesting code snippets &amp; experiments

### [parallel_sort.h](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h)
Sorts by splitting the input into pieces, sorting the pieces individually (and simultaneously, in parallel) and and then also merging _in parallel_. Relies on `std::sort` and `std::inplace_merge` and only manages and organizes the multithreaded work. See [`impl::merge_threads()`](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h#L116) for details.
### Reservoir sampling ([.hs](https://github.com/Andreshk/SnippySnippets/blob/master/ReservoirSampling.hs), [.h](https://github.com/Andreshk/SnippySnippets/blob/master/reservoir_sampling.h))
Implementations of two algorithms for the classic problem of reservoir sampling: randomly choosing a fixed (typically, small) number `k` of values from a stream of unknown size `n` in O(k) space so that every value has the same probability of being sampled. This invariant holds at every point in time, regardless of the number of processed values from the stream (i.e. in the reservoir). The two algorithms are: the naive, O(n) one; and an O(k*log(n/k)) one, which is optimal for the task.
### [Graph](https://github.com/Andreshk/SnippySnippets/blob/master/Graph.hs), [TopoSort](https://github.com/Andreshk/SnippySnippets/blob/master/TopoSort.hs)
Topological sorting (based on depth-first search), implemented in Haskell using the State monad (and Vector-s for a small performance gain).
### [VectorTest](https://github.com/Andreshk/SnippySnippets/blob/master/VectorTest.hs)
Some experiments with mutable vectors in Haskell
### [Cutting Stock Pattern Generator](https://github.com/Andreshk/SnippySnippets/blob/master/cutting_stock_pattern_generator.h)
An exercise in C++20 coroutines for lazily generating all possible patterns for the [cutting stock problem](https://en.wikipedia.org/wiki/Cutting_stock_problem) one by one, from synchronous coroutines (a recursive & non-recursive one). This is generally a good approach for such problems, where a lot of permutations/combinations have to be generated, the generation is stateful (often the next combination is generated from the previous one), but we don't need to store all of them in memory at once. Uses the generators from [`cppcoro`](https://github.com/lewissbaker/cppcoro) as helper utilities.
