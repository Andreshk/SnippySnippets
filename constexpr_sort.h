#pragma once
#include<array>

namespace constexpr_ {
template<class T, size_t N>
constexpr bool is_sorted(const std::array<T, N>& arr)
{
    for (size_t i = 0; i < N - 1; ++i)
        if (arr[i] > arr[i + 1])
            return false;
    return true;
}

template<class T>
constexpr void swap(T& x, T& y)
{
    T tmp = std::move(y);
    y = std::move(x);
    x = std::move(tmp);
}

template<class T>
constexpr void insertion_sort_impl(T* const arr, size_t from, size_t to)
{
    for (size_t i = from + 1; i < to; ++i) {
        const T x = arr[i];
        size_t j = i - 1;
        while (j < i && arr[j] > x) { // j < i instead of j >= 0
            arr[j + 1] = arr[j];
            --j;
        }
        arr[j + 1] = x;
    }
}

template<class T>
constexpr size_t partition(T* const arr, size_t from, size_t to)
{
    const T& pivot = arr[from];
    size_t i = from - 1, j = to;
    while (true) {
        do { ++i; } while (arr[i] < pivot);
        do { --j; } while (arr[j] > pivot);
        if (i >= j) { return j; }
        swap(arr[i], arr[j]);
    }
}

template<class T>
constexpr void quick_sort_impl(T* const arr, size_t from, size_t to)
{
    // Recurse down for the smaller piece of the array (for a
    // shallower call stack); and then iterate on the other
    // piece until it's small enough to sort naively.
    while (to - from > 10) {
        const size_t pivot = partition(arr, from, to);
        if (pivot - from < to - pivot) {
            quick_sort_impl(arr, from, pivot + 1);
            from = pivot + 1;
        } else {
            quick_sort_impl(arr, pivot + 1, to);
            to = pivot + 1;
        }
    }
    insertion_sort_impl(arr, from, to);
}

template<class T, size_t N>
constexpr std::array<T, N> quick_sort(const std::array<T, N>& arr)
{
    std::array<T, N> res{ arr };
    quick_sort_impl(res.data(), 0, res.size());
    return res;
}

template<class T, size_t N>
constexpr std::array<T, N> insertion_sort(const std::array<T, N>& arr)
{
    std::array<T, N> res{ arr };
    insertion_sort_impl(res.data(), 0, res.size());
    return res;
}

template<class T, size_t N>
constexpr std::array<T, N> bubble_sort(const std::array<T, N>& arr)
{
    std::array<T, N> res{ arr };
    for (size_t i = 0; i < N - 1; ++i)
        for (size_t j = i + 1; j < N; ++j)
            if (res[i] > res[j])
                swap(res[i], res[j]);
    return res;
}
} // namespace constexpr_
