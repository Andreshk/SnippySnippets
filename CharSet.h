#pragma once
#include <cstdint>
#include <utility> // std::exchange
#include <bit> // std::popcount, std::countr_zero

class CharSet {
    uint64_t data[2];
public:
    using Index = char;
    CharSet() : data{ 0,0 } {}
    CharSet(const CharSet&) = default;
    CharSet& operator=(const CharSet&) = default;
    CharSet(CharSet&& other) noexcept : data{ other.data[0], other.data[1] } { other.clear(); }
    CharSet& operator=(CharSet&& other) noexcept {
        data[0] = std::exchange(other.data[0], 0); // no need for `if (this != &other)` checks this way
        data[1] = std::exchange(other.data[1], 0);
        return *this;
    }
    ~CharSet() { clear(); }
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
    // Iteration-related methods - use a copy of the object itself as an iterator.
    // Dereferencing it returns the smallest char in the set, advancing removes said char from the set.
    // We can't get a smaller, more efficient iterator than this :)
    using iterator = CharSet;
    using const_iterator = iterator;
    iterator begin() const noexcept { return *this; }
    iterator end() const noexcept { return CharSet{}; }
    void operator++() noexcept {
        data[data[0] == 0] &= (data[data[0] == 0] - 1); // lol
    }
    Index operator*() const noexcept {
        return Index(64 * (data[0] == 0) + std::countr_zero(data[data[0] == 0]));
    }
    bool operator==(const CharSet&) const noexcept = default;
};
