#pragma once
#include <initializer_list>

namespace constexpr_ {
template<class T, size_t N>
class array
{
    T data[N]{};
public:
    constexpr array() {}
    constexpr array(std::initializer_list<T> il) {
        // an ugly alternative to static_assert(il.size() == N);
        if (il.size() != N) throw std::range_error{ "Size mismatch!" };
        for (size_t i = 0; i < N; ++i)
            data[i] = il.begin()[i];
    }
    constexpr       T& operator[](size_t idx)       { return data[idx]; }
    constexpr const T& operator[](size_t idx) const { return data[idx]; }
    constexpr static size_t size() { return N; }
    
    using iterator = T*;
    constexpr iterator begin() { return &data[0]; }
    constexpr iterator   end() { return &data[N]; }
    using const_iterator = const T*;
    constexpr const_iterator begin() const { return &data[0]; }
    constexpr const_iterator   end() const { return &data[N]; }
};
} // namespace constexpr_
