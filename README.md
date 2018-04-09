# SnippySnippets
A repository for random, useful or interesting code snippets &amp; experiments

### constexpr_\*
A constexpr implementation of [`std::array`](https://github.com/Andreshk/SnippySnippets/blob/master/constexpr_array.h), since it is constexpr only as of C++17; and a couple of [constexpr sorting](https://github.com/Andreshk/SnippySnippets/blob/master/constexpr_sort.h) algorithms for proof-of-concept. As mentioned, `constexpr_::array` can be used in place of `std::array` for almost all constexpr purposes.
### [parallel_sort.h](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h)
Sorts by splitting the input into pieces, sorting the pieces individually (and simultaneously, in parallel) and and then also merging _in parallel_. Relies on `std::sort` and `std::inplace_merge` and only manages and organizes the multithreaded work. See [`impl::merge_threads()`](https://github.com/Andreshk/SnippySnippets/blob/master/parallel_sort.h#L123) for details.
