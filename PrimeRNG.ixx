export module PrimeRNG;
export import <cstdint>;
import <concepts>;  // std::{unsigned_integral,same_as}
import <random>;    // std::{uniform_random_bit_generator,random_device}
import <algorithm>; // std::ranges::{contains,none_of}
import <span>;
import <immintrin.h>; // AVX2 stuff
import MillerRabin;

// Adapts a uniform random bit generator to deterministically generate prime numbers of the given type.
export template <std::uniform_random_bit_generator Gen, std::unsigned_integral U>
    requires !std::same_as<U, uint64_t>
class PrimeRNG {
    Gen gen;

    // Returns a small set of primes for filtering out some composites before the heavy-duty Miller-Rabin
    static std::span<const __m256i> getPrimes() {
        if constexpr (std::same_as<U, uint8_t>) {
            // All of the 8 bit primes, padded up to a multiple of 16
            alignas(__m256i) static const uint8_t ps[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
                43, 47, 53,	59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
                137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
                227, 229, 233, 239, 241, 251, 251, 251, 251, 251, 251, 251, 251, 251, 251, 251 };
            static_assert(sizeof(ps) % sizeof(__m256i) == 0);
            return { (const __m256i*)ps, sizeof(ps) / sizeof(__m256i) };
        } else {
            alignas(__m256i) static const U ps[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
                43, 47, 53,	59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131,
                137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
                227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311 };
            static_assert(sizeof(ps) % sizeof(__m256i) == 0);
            return { (const __m256i*)ps, sizeof(ps) / sizeof(__m256i) };
        }
    }
    // Waiting for std::simd to relieve us of this boilerplate
    static __m256i set1(const U n) {
        return (std::same_as<U, uint8_t> ? _mm256_set1_epi8(n)
                : (std::same_as<U, uint16_t> ? _mm256_set1_epi16(n)
                   : _mm256_set1_epi32(n)));
    }
    static bool any_cmpeq(const __m256i a, const __m256i b) {
        const __m256i res = (std::same_as<U, uint8_t> ? _mm256_cmpeq_epi8(a, b)
                              : (std::same_as<U, uint16_t> ? _mm256_cmpeq_epi16(a, b)
                                 : _mm256_cmpeq_epi32(a, b)));
        return (_mm256_movemask_epi8(res) != 0);
    }
    static __m256i round(const __m256i a, const __m256i b) /*requires !std::same_as<U, uint8_t>*/ {
        return (std::same_as<U, uint16_t> ? _mm256_mullo_epi16(b, _mm256_div_epu16(a, b))
                : _mm256_mullo_epi32(b, _mm256_div_epu32(a, b)));
    }
public:
    PrimeRNG(const uint64_t seed = std::random_device{}()) : gen{ seed } {}
    struct Result { U n; int it; };
    // Returns a randomly-generated prime number together with the # of calls to the RNG it took finding it.
    Result operator()() {
        const std::span<const __m256i> primes = getPrimes();
        const U last = reinterpret_cast<const U*>(&primes.back() + 1)[-1];
        for (int it = 1; ; ++it) {
            const U n = U(gen()); // This may discard many bits - hope your engine is fast
            const bool isPrime = (std::same_as<U, uint8_t> || n <= last
                ? std::ranges::any_of(primes, // This is std::ranges::contains(primes, n)
                    [nn = set1(n)](const __m256i pi) { return any_cmpeq(nn, pi); })
                : std::ranges::none_of(primes, // This is std::ranges::none_of(primes, [&](int p) { return (n % p == 0); })
                    [nn = set1(n)](const __m256i pi) { return any_cmpeq(nn, round(nn, pi)); })
                  && millerRabin(n));
            if (isPrime) {
                return { n,it };
            }
        }
    }
};
