export module SplitMix;
// SplitMix uses & returns the imported type => reexport it to be visible at import site
export import <cstdint>; // uint64_t
import <random>; // std::{random_device,uniform_random_bit_generator}
import <bit>; // std::bit_cast

// A simple 64-bit PRNG using 64 bits of state
// Models std::uniform_random_bit_generator, so can be used as an engine with std:: distributions
export class SplitMix64 {
	uint64_t s;
public:
	SplitMix64(const uint64_t seed = std::random_device{}()) : s{ seed } {}
	uint64_t operator()() {
		uint64_t z = (s += 0x9e3779b97f4a7c15);
		z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
		z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
		return z ^ (z >> 31);
	}
	// For std::uniform_random_bit_generator
	constexpr static uint64_t min() { return 0; }
	constexpr static uint64_t max() { return -1; }
};

// Wrapper around the SplitMix64 generator that outputs uniform doubles in [0;1).
// Obviously not a std::uniform_random_bit_generator, but incredibly useful directly.
export class SplitMix64F {
	SplitMix64 gen;
public:
	SplitMix64F(const uint64_t seed = std::random_device{}()) : gen{ seed } {}
	double operator()() {
		// This makes an uniformly distributed number in [1;2) to avoid dealing with subnormals.
		// Also, using the higher bit of gen() is generally a good practice.
		const uint64_t x = (0x3FFull << 52 | gen() >> 12);
		// Subtracting 1 makes it uniformly distributed in [0;1). Since [0;1) is twice denser,
		// the subtraction is safe, but only every second number in it can be obtained.
		// This is a common property of floating-point generators and not an issue.
		return std::bit_cast<double>(x) - 1.0;
	}
};
// Sanity checks
static_assert(std::uniform_random_bit_generator<SplitMix64>);
static_assert(!std::uniform_random_bit_generator<SplitMix64F>);
