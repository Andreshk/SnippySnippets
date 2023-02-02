export module Xoshiro;
// The generators use & return these imported types => reexport them to be visible at import site
export import <cstdint>; // uint32_t,uint64_t
import <concepts>;
import <random>; // std::{random_device,uniform_random_bit_generator}
import <array>;
import <bit>; // std::{bit_cast,rotl}
import <cassert>;
import SplitMix;

// Fast and high-quality PRNGs, using 128 bits of state for generation of 32-bit values and 256 bits of state for generation of 64-bit values.
// The integral generators model std::uniform_random_bit_generator, so can be used as an engines with std:: distributions.
// The floating-point generators directly return uniformly distributed values in [0;1) faster than combining en engine above with a std:: distribution.
// See the typedefs below for the most common uses.
export template <typename Result, bool B = true>
	requires (std::is_arithmetic_v<Result> && (sizeof(Result) == 4 || sizeof(Result) == 8))
class Xoshiro {
	static constexpr int N = 32 * sizeof(Result); // state size in bits, also determines result type
	using Value = std::conditional_t<N == 128, uint32_t, uint64_t>;
	std::array<Value, 4> s;
	// Helper function for "jumping" to some future state after however many calls to operator()
	void jumpWith(const std::array<Value, 4> jump) {
		static_assert(!std::floating_point<Result>, "to-do");
		std::array<Value, 4> sNew = {};
		for (const Value j : jump) {
			for (int b = 0; b < (N / 4); b++) { // # of bits in Value
				if (j & Value(1) << b) {
					sNew[0] ^= s[0];
					sNew[1] ^= s[1];
					sNew[2] ^= s[2];
					sNew[3] ^= s[3];
				}
				(*this)();
			}
		}
		s = sNew;
	}
public:
	// Seed with a single value, passed through the more simple SplitMix64 to fill the entire state
	Xoshiro(const uint64_t seed = std::random_device{}()) {
		SplitMix64 mix{ seed };
		if constexpr (N == 128) {
			std::array<uint64_t, 2> tmp = { mix(), mix() };
			s = std::bit_cast<std::array<uint32_t, 4>>(tmp);
		} else for (uint64_t& val : s) {
			val = mix();
		}
		assert(s[0] * s[1] * s[2] * s[3] != 0); // state should not be all zero
		// Sanity check
		static_assert(std::integral<Result> == std::uniform_random_bit_generator<Xoshiro>);
	}
	// No copying/moving around, please
	Xoshiro(const Xoshiro&) = delete;
	Xoshiro& operator=(const Xoshiro&) = delete;
	Xoshiro(Xoshiro&&) = delete;
	Xoshiro& operator=(Xoshiro&&) = delete;
	// Generate a single value
	Result operator()() {
		Value result = 0;
		if constexpr (std::floating_point<Result>) {
			result = s[0] + s[3];
		} else if constexpr (B) {
			result = std::rotl(s[1] * 5, 7) * 9;
		} else {
			result = std::rotl(s[0] + s[3], (N == 128 ? 7 : 23)) + s[0];
		}
		const Value t = s[1] << (N == 128 ? 9 : 17);
		s[2] ^= s[0];
		s[3] ^= s[1];
		s[1] ^= s[2];
		s[0] ^= s[3];
		s[2] ^= t;
		s[3] = std::rotl(s[3], (N == 128 ? 11 : 45));
		if constexpr (std::integral<Result>) {
			static_assert(std::same_as<Result, Value>);
			return result;
		} else if constexpr (N == 128) { // See comment in SplitMix64F
			result = (0x7Fu << 23 | result >> 9);
			return std::bit_cast<float>(result) - 1.0f;
		} else {
			result = (0x3FFull << 52 | result >> 12);
			return std::bit_cast<double>(result) - 1.0;
		}
	}
	// A jump is equivalent to 2^(N/2) calls to operator() and can be used
	// to generate 2^(N/2) non-overlapping subsequences for parallel computations.
	void jump() {
		static_assert(!std::floating_point<Result>, "to-do");
		if constexpr (N == 128) {
			jumpWith({ 0x8764000b, 0xf542d2d3, 0x6fa035c3, 0x77f2db5b });
		} else {
			jumpWith({ 0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c });
		}
	}
	// A long jump is equivalent to 2^(3N/4) calls to operator() and can be used to
	// generate 2^(N/4) starting points, from each of which jump() will generate
	// 2^(N/4) non-overlapping subsequences for parallel distributed computations.
	void long_jump() {
		static_assert(!std::floating_point<Result>, "to-do");
		if constexpr (N == 128) {
			jumpWith({ 0xb523952e, 0x0b6f099f, 0xccf5a0ef, 0x1c580662 });
		} else {
			jumpWith({ 0x76e15d3efefdcbbf, 0xc5004e441c522fb3, 0x77710069854ee241, 0x39109bb02acbe635 });
		}
	}
	// For std::uniform_random_bit_generator
	constexpr static Result min() requires std::integral<Result> { return 0; }
	constexpr static Result max() requires std::integral<Result> { return -1; }
};

// Convenience aliases
export using Xoshiro128 = Xoshiro<uint32_t>; // xoshiro128**
export using Xoshiro256 = Xoshiro<uint64_t>; // xoshiro256**
export using Xoshiro128F = Xoshiro<float>;  // xoshiro128+
export using Xoshiro256F = Xoshiro<double>; // xoshiro256+
export using Xoshiro128P = Xoshiro<uint32_t, false>; // xoshiro128++, slightly inferior to xoshiro128**
export using Xoshiro256P = Xoshiro<uint64_t, false>; // xoshiro256++, slightly inferior to xoshiro256**
