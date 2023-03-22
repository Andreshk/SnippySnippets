export module PrimeRNG;
export import <cstdint>;
import <concepts>;  // std::{unsigned_integral,same_as}
import <random>;    // std::{uniform_random_bit_generator,random_device}
import <bit>;       // std::countr_zero
import <algorithm>; // std::ranges::{contains,none_of}

// Adapts a uniform random bit generator to deterministically generate prime numbers of the given type.
export template <std::uniform_random_bit_generator Gen, std::unsigned_integral U>
class PrimeRNG {
	Gen gen;

	// See "Testing against small sets of bases" section in wiki
	static std::initializer_list<U> bases() {
		if constexpr (std::same_as<U, uint8_t> || std::same_as<U, uint16_t>) {
			return { 2,3 };
		} else if constexpr (std::same_as<U, uint32_t>) {
			return { 2,7,61 };
		} else {
			return { 2,3,5,7,11,13,17,19,23,29,31,37 };
		}
	}
	// Calculates (a*b)%m without overflow - easy for all types except uint64_t
	static U mul(U a, U b, const U m) {
		if constexpr (std::same_as<U, uint8_t>) {
			return uint8_t(uint16_t(a) * b % m);
		} else if constexpr (std::same_as<U, uint16_t>) {
			return uint16_t(uint32_t(a) * b % m);
		} else if constexpr (std::same_as<U, uint32_t>) {
			return uint32_t(uint64_t(a) * b % m);
		} else {
			uint64_t res = 0;
			while (a != 0) {
				if (a & 1) {
					res = (res + b) % m;
				}
				a >>= 1;
				b = (b << 1) % m;
			}
			return res;
		}
	}
	// Calculates (a^b)%m without overflow, using fast exponentiation
	static U pow(U a, U b, const U m) {
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
	// Deterministically tests whether n is a prime number
	static bool millerRabin(const U n) {
		const int s = std::countr_zero(U(n - 1));
		const U d = (n - 1) >> s;
		return std::ranges::none_of(bases(),
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
public:
	PrimeRNG(const uint64_t seed = std::random_device{}()) : gen{ seed } {}
	struct Result { U n; int it; };
	// Returns a randomly-generated prime number together with the # of calls to the RNG it took finding it.
	Result operator()() {
		// Small set of primes for quick testing
		static const unsigned primes[] = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
			31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103,
			107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179,
			181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257,
			263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347 };
		const unsigned last = primes[std::size(primes) - 1];
		for (int it = 1; ; ++it) {
			const U n = U(gen()); // If U is less than 64-bit, this truncates :/
			// Small enough numbers can be checked from the list, and we can otherwise
			// use it for a preliminary filter before the heavy-duty Miller-Rabin test
			const bool isPrime = (n <= last
				? std::ranges::contains(primes, n)
				: std::ranges::none_of(primes, [&](unsigned p) { return n % p == 0; }) && millerRabin(n));
			if (isPrime) {
				return { n,it };
			}
		}
	}
};
