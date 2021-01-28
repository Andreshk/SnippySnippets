# SnippySnippets
A repository for random, useful or interesting code snippets &amp; experiments

### constexpr_\*
A constexpr implementation of [`std::array`](https://github.com/Andreshk/SnippySnippets/blob/master/constexpr_array.h), since it is constexpr only as of C++17; and a couple of [constexpr sorting](https://github.com/Andreshk/SnippySnippets/blob/master/constexpr_sort.h) algorithms for proof-of-concept. As mentioned, `constexpr_::array` can be used in place of `std::array` for almost all constexpr purposes. Most importantly, the `constexpr_::` sorting functions can be called with both types of arrays and work during compile-time.
### [parallel_sort.h](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h)
Sorts by splitting the input into pieces, sorting the pieces individually (and simultaneously, in parallel) and and then also merging _in parallel_. Relies on `std::sort` and `std::inplace_merge` and only manages and organizes the multithreaded work. See [`impl::merge_threads()`](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h#L116) for details.
### [Reservoir sampling](https://github.com/Andreshk/SnippySnippets/blob/master/ReservoirSampling.hs)
A classic, simple algorithm (or, rather, a data structure called a 'sampler') for randomly choosing a fixed (typically, small) number of values from a stream of unknown, possibly infinite size, in such a way that every value, added to the sampler, has the same probability of being sampled. This guarantee holds at every point in time, regardless of the number of values already added to a sampler.
### [Graph](https://github.com/Andreshk/SnippySnippets/blob/master/Graph.hs), [TopoSort](https://github.com/Andreshk/SnippySnippets/blob/master/TopoSort.hs)
Topological sorting (based on depth-first search), implemented in Haskell using the State monad (and Vector-s for a small performance gain).
