export module MillerRabin;
import <cstdint>;
import <concepts>;  // std::{unsigned_integral,same_as}
import <algorithm>; // std::ranges::none_of
import <bit>;       // std::countr_zero

// Calculates (a*b)%m without overflow - easy for all types except uint64_t
template <std::unsigned_integral U>
U mul(const U a, const U b, const U m) {
    if constexpr (std::same_as<U, uint8_t>) {
        return uint8_t(uint16_t(a) * b % m);
    } else if constexpr (std::same_as<U, uint16_t>) {
        return uint16_t(uint32_t(a) * b % m);
    } else {
        return uint32_t(uint64_t(a) * b % m);
    }
}
// Calculates (a^b)%m without overflow, using fast exponentiation
template <std::unsigned_integral U>
U pow(U a, U b, const U m) {
    U x = 1;
    a %= m;
    while (b > 0) {
        if (b % 2 == 1) {
            x = mul(x, a, m); // multiplying with base
        }
        a = mul(a, a, m); // squaring the base
        b >>= 1;
    }
    return x % m;
}
// See "Testing against small sets of bases" section in wiki
template <std::unsigned_integral U>
std::initializer_list<U> bases() {
    if constexpr (std::same_as<U, uint8_t> || std::same_as<U, uint16_t>) {
        return { 2,3 };
    } else {
        return { 2,7,61 };
    }
}
// Deterministically tests whether n is a prime number.
export template <std::unsigned_integral U>
    requires (!std::same_as<U, uint64_t>)
bool millerRabin(const U n) {
    const int s = std::countr_zero(U(n - 1));
    const U d = (n - 1) >> s;
    return std::ranges::none_of(bases<U>(),
        [&](const U a) {
            U x = pow(a, d, n);
            U y = -1;
            for (int i = 0; i < s; ++i) {
                y = mul(x, x, n);
                if (y == 1 && x != 1 && x != (n - 1)) {
                    return true;
                }
                x = y;
            }
            return (y != 1);
        });
}
