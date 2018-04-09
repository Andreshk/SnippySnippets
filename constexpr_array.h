#pragma once
#include <initializer_list>

namespace constexpr_ {
template<class T, size_t N>
class array
{
    T _data[N]{};
public:
    constexpr array() {}
    constexpr array(std::initializer_list<T> il) {
        // an ugly alternative to static_assert(il.size() == N);
        if (il.size() != N) throw std::range_error{ "Size mismatch!" };
        for (size_t i = 0; i < N; ++i)
            _data[i] = il.begin()[i];
    }
    constexpr       T& operator[](size_t idx)       { return _data[idx]; }
    constexpr const T& operator[](size_t idx) const { return _data[idx]; }
    constexpr static size_t size() noexcept { return N; }
    
    using iterator = T*;
    constexpr iterator begin() noexcept { return &_data[0]; }
    constexpr iterator   end() noexcept { return &_data[N]; }
    using const_iterator = const T*;
    constexpr const_iterator begin() const noexcept { return &_data[0]; }
    constexpr const_iterator   end() const noexcept { return &_data[N]; }
    constexpr       T* data()       noexcept { return _data; }
    constexpr const T* data() const noexcept { return _data; }
};
} // namespace constexpr_
