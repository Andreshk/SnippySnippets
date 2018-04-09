#include "constexpr_sort.h"
#include <iostream>

using namespace constexpr_;
template<class T, size_t N>
void print(const std::array<T, N>& arr) {
    for (const auto& x : arr)
        std::cout << x << ' ';
    std::cout << '\n';
}

int main()
{
    constexpr std::array<int, 5> arr{ 1,4,2,5,3 };
    static_assert(is_sorted(insertion_sort(arr)));
    static_assert(is_sorted(bubble_sort(arr)));
    static_assert(is_sorted(quick_sort(arr)));

    std::array<int, 50> arr2;
    for (size_t i = 0; i < arr2.size(); ++i)
        arr2[i] = ((3672 * i * i) - 42 * i + 25) % 47;
    auto arr3 = quick_sort(arr2);
    //print(arr3);
    std::cout << (is_sorted(arr3) ? "true" : "false") << "\n";
}