#pragma once
#include <vector>
#include <cstdint>
#include <intrin.h> // __popcnt()

class BitVector {
public:
    // You can change this manually in order to test/visualize stuff
    static const size_t w = sizeof(size_t) * CHAR_BIT;
    static_assert(w == 16 || w == 32 || w == 64);
    using value_type = std::conditional_t<w == 16, uint16_t,
                         std::conditional_t<w == 32, uint32_t, uint64_t>>;
private:
    std::vector<value_type> bits;
    size_t n;
    static value_type popcount(value_type x) {
        if constexpr (w == 16)
            return __popcnt16(uint16_t(x));
        else if constexpr (w == 32)
            return __popcnt(uint32_t(x));
        else
            return __popcnt64(x);
    }
public:
    explicit BitVector(size_t n = 0) : bits(n / w + (n%w != 0), 0), n(n) {}
    bool operator[](size_t i) const {
        return ((bits[i / w] & (value_type(1) << (w - 1 - (i%w)))) != 0);
    }
    void set(size_t i) {
        bits[i / w] |= (value_type(1) << (w - 1 - (i%w)));
    }
    void clear(size_t i) {
        bits[i / w] &= ~(value_type(1) << (w - 1 - (i%w)));
    }
    void push_back(bool b) {
        if (n%w == 0)
            bits.push_back(0);
        if (b)
            set(n);
        ++n;
    }
    size_t size() const {
        return n;
    }
    BitVector& operator|=(const BitVector& rhs) {
        // If rhs is larger, resize to match it
        if (rhs.bits.size() > bits.size()) {
            bits.resize(rhs.bits.size(), 0);
            n = rhs.n;
        }
        for (size_t i = 0; i < bits.size(); ++i)
            bits[i] |= rhs.bits[i];
        return *this;
    }
    size_t rank(size_t i) { // Number of set bits among the first n, unfortunately O(n/w)
        size_t result = 0;
        for (size_t j = 0; j < i / w; ++j) {
            result += popcount(bits[j]);
        }
        result += popcount(bits[i / w] >> (w - (i%w))); // no -1 here, due to 1-indexed counting
        return result;
    }
};
