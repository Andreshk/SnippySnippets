#pragma once
#include <type_traits> // std::conditional_t
#include <cstdint>
#include <utility> // std::exchange
#include <bit> // std::popcount, std::countr_zero

// Forward declaration - a bitset can contain only a power-of-2 number
// of bits, corresponding to the size of a standard integral type or 128.
// For 128 there is an explicit instantiation below the main one.
template <size_t N>
    requires (N == 8 || N == 16 || N == 32 || N == 64 || N == 128)
class StaticBitset;

// Use a copy of the bitset as an iterator to its values - NOT for direct use!
// Dereferencing it returns the smallest value in the set, advancing removes said value from the set.
// We can't get a smaller, more efficient iterator than this :)
template <size_t N>
class StaticBitsetIterator {
    StaticBitset<N> b;
    // Only a bitset can create iterators to itself
    friend StaticBitset<N>;
    StaticBitsetIterator(const StaticBitset<N>& b) : b{ b } {}
public:
    StaticBitsetIterator() = default;
    void operator++() noexcept { b.popFront(); }
    auto operator*() const noexcept { return b.front(); }
    bool operator==(const StaticBitsetIterator&) const noexcept = default;
};

template <size_t N>
    requires (N == 8 || N == 16 || N == 32 || N == 64 || N == 128)
class StaticBitset {
    // The standard unsigned integral type with N bits
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
    // Methods for returning/removing the smallest value in the set
    Index front() const noexcept { return Index(std::countr_zero(data)); }
    void popFront() noexcept { data &= (data - 1); }
    // Iteration-related methods
    using iterator = StaticBitsetIterator<N>;
    iterator begin() const noexcept { return { *this }; }
    iterator end() const noexcept { return {}; }
    // In case anyone needs to hash/compare sets
    bool operator==(const StaticBitset&) const noexcept = default;
    auto operator<=>(const StaticBitset&) const noexcept = default;
};

template <>
class StaticBitset<128> {
    uint64_t data[2];
public:
    using Index = uint8_t;
    StaticBitset() : data{ 0,0 } {}
    StaticBitset(const StaticBitset&) = default;
    StaticBitset& operator=(const StaticBitset&) = default;
    StaticBitset(StaticBitset&& other) noexcept : data{ other.data[0], other.data[1] } { other.clear(); }
    StaticBitset& operator=(StaticBitset&& other) noexcept {
        data[0] = std::exchange(other.data[0], 0); // no need for `if (this != &other)` checks
        data[1] = std::exchange(other.data[1], 0);
        return *this;
    }
    ~StaticBitset() { clear(); }
    // Standard set operations
    bool contains(const Index c) const noexcept {
        return bool(data[c / 64] & (1ull << (c % 64)));
    }
    void add(const Index c) noexcept {
        data[c / 64] |= (1ull << (c % 64));
    }
    void remove(const Index c) noexcept {
        data[c / 64] &= ~(1ull << (c % 64));
    }
    // Rank (or index) of c in the set <=> # of chars less than c <=> # of raised bits before the c-th
    size_t rank(const Index c) const noexcept {
        if (c == 0) { // Avoid left-shifting by 64 in the case below - the result is undefined (!)
            return 0;
        } else if (c < 64) {
            return std::popcount(data[0] << (64 - c));
        } else {
            return std::popcount(data[0]) + std::popcount(data[1] << (128 - c));
        }
    }
    // More standard methods
    size_t size() const noexcept { return (std::popcount(data[0]) + std::popcount(data[1])); }
    bool empty() const noexcept { return (data[0] == 0 && data[1] == 0); }
    void clear() noexcept { data[0] = data[1] = 0; }
    // Methods for returning/removing the smallest value in the set
    Index front() const noexcept {
        return Index(64 * (data[0] == 0) + std::countr_zero(data[data[0] == 0]));
    }
    void popFront() noexcept {
        data[data[0] == 0] &= (data[data[0] == 0] - 1); // lol
    }
    // Iteration-related methods
    using iterator = StaticBitsetIterator<128>;
    iterator begin() const noexcept { return { *this }; }
    iterator end() const noexcept { return {}; }
    // In case anyone needs to hash/compare sets
    bool operator==(const StaticBitset&) const noexcept = default;
    auto operator<=>(const StaticBitset&) const noexcept = default;
};
