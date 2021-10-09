#pragma once
#include <concepts> // std::unsigned_integral
#include <cstdint> // uint8_t
#include <utility> // std::exchange
#include <bit> // std::popcount, std::countr_zero

template <size_t N>
class StaticBitset {
    static_assert(N == 8 || N == 16 || N == 32 || N == 64);
    using T = std::conditional_t<N == 8, uint8_t,
              std::conditional_t<N == 16, uint16_t,
              std::conditional_t<N == 32, uint32_t, uint64_t>>>;
    static constexpr size_t numBits = sizeof(T) * CHAR_BIT;
    T data;
public:
    using Index = uint8_t;
    StaticBitset() : data{ 0 } {}
    StaticBitset(const StaticBitset&) = default;
    StaticBitset& operator=(const StaticBitset&) = default;
    StaticBitset(StaticBitset&& other) noexcept : data{ std::exchange(other.data, 0) } {}
    StaticBitset& operator=(StaticBitset&& other) noexcept {
        data = std::exchange(other.data, 0); // no need for (this != &other) checks
        return *this;
    }
    ~StaticBitset() { clear(); }
    // Standard set operations
    bool contains(const Index c) const noexcept {
        return bool(data & (T(1) << c));
    }
    void add(const Index c) noexcept {
        data |= (T(1) << c);
    }
    void remove(const Index c) noexcept {
        data &= ~(T(1) << c);
    }
    // Rank (or index) if c in the set <=> # of chars less than c <=> # of raised bits before the c-th
    size_t rank(const Index c) const noexcept {
        // Avoid left-shifting by numBits - the result is undefined (!)
        return (c == 0 ? 0 : std::popcount(data << (numBits - c)));
    }
    // More standard methods
    size_t size() const noexcept { return std::popcount(data); }
    bool empty() const noexcept { return (data == 0); }
    void clear() noexcept { data = 0; }
    // Iteration-related methods - use a copy of the object itself as an iterator.
    // Dereferencing it returns the smallest value in the set, advancing removes said value from the set.
    // We can't get a smaller, more efficient iterator than this :)
    using iterator = StaticBitset;
    using const_iterator = iterator;
    iterator begin() const noexcept { return *this; }
    iterator end() const noexcept { return StaticBitset{}; }
    void operator++() noexcept { data &= (data - 1); }
    Index operator*() const noexcept { return Index(std::countr_zero(data)); }
    bool operator==(const StaticBitset&) const noexcept = default;
};
