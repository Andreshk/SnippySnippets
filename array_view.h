#pragma once
#include <array>
#include <type_traits> // std::enable_if_t, std::index_sequence

#include <cassert>
#define vassert assert

// Helper function for subarray generation from a std::array
template <typename T, size_t N, size_t... Idxs>
std::array<T, N - 1> subarray_impl(const std::array<T, N>& arr, std::index_sequence<Idxs...>) {
    return { arr[Idxs + 1]... };
}
// Generates a subarray, containing all but the first element of the given std::array
template <typename T, size_t N>
std::array<T, N - 1> subarray(const std::array<T, N>& arr) {
    static_assert(N >= 1);
    return subarray_impl(arr, std::make_index_sequence<N - 1>{});
}
// Product of the values in a given std::array, starting from a given offset.
template <size_t N>
auto product(const std::array<size_t, N>& arr, const size_t offset = 0) {
    size_t res = 1;
    for (size_t i = offset; i < N; ++i) {
        res *= arr[i];
    }
    return res;
}

// Multi-dimensional ArrayView (a.k.a. span). Built from a raw pointer, with operator[]
// generating a smaller array view. This way multi-dimensional accesses can be guarded
// for out-of-bounds. Note: As a consequence, operator[] is not a trivial operation,
// so using arr[i][j] in a tight loop is not recommended (!)
template <typename T, size_t N = 1>
class ArrayView {
    static_assert(N >= 1, "0-dimensional arrays are not allowed.");

    T* ptr;
    std::array<size_t, N> sizes;
public:
    // We'd like to have a simple constructor for the one-dimensional case: ArrayView<int>(ptr, 20)
    template <size_t N1 = N, typename = std::enable_if_t<N1 == 1>>
    ArrayView(T* ptr, const size_t n) : ptr{ ptr }, sizes{ n } {};
    // Same goes for a constructor from an iterator pair: ArrayView<int>(vec.begin(), vec.end())
    template <typename RandomIt, size_t N1 = N, typename = std::enable_if_t<N1 == 1>>
    ArrayView(RandomIt from, RandomIt to) : ptr{ std::addressof(*from) }, sizes{ size_t(to - from) } {};
    // Otherwise, the sizes of each subdimension must be given: ArrayView<int,3>(ptr,{2,3,5});
    ArrayView(T* ptr, const std::array<size_t, N> sizes) : ptr{ ptr }, sizes{ sizes } {};

    // The size of a given subdimension (by default, the number of N-1 subarrays)
    size_t size(const size_t subDim = 0) const {
        return sizes[subDim];
    }
    // The total number of values contained
    size_t totalSize() const {
        return product(sizes);
    }
    // The dimensionality of an array view
    size_t numDimensions() const {
        return N;
    }

    // One-dimensional array operator[] accesses the values directly and returns T& or const T&.
    // Multi-dimensional array operator[] creates (!) a new ArrayView<T,N-1> or ArrayView<const T,N-1>.
    decltype(auto) operator[](const size_t i) {
        vassert(i < size());
        if constexpr (N == 1) {
            return ptr[i];
        } else {
            const size_t subDimSize = product(sizes, 1);
            return ArrayView<T, N - 1>{ ptr + i*subDimSize, subarray(sizes) };
        }
    }
    decltype(auto) operator[](const size_t i) const {
        vassert(i < size());
        if constexpr (N == 1) {
            return reinterpret_cast<const T&>(ptr[i]);
        } else {
            const size_t subDimSize = product(sizes, 1);
            return ArrayView<const T, N - 1>{ ptr + i*subDimSize, subarray(sizes) };
        }
    }
};
