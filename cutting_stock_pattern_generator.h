#include <cppcoro/generator.hpp>
#include <cppcoro/recursive_generator.hpp>
#include <vector>
#include <span>

// Coroutines, generating all possible patterns for a cutting stock problem.
// This, of course, is a minor step in the process of solving this problem.
// Each pattern is identified by the number of occurences of each item.

namespace detail {
// Helper recursive coroutine that signals each time an entire new pattern is generated.
// Max recursion depth is equal to the number of items (yikes) - should be taken as a proof of concept only.
// Note: int is used as a dummy type, should be void ot std::monostate
cppcoro::recursive_generator<int> helper(const int S, std::span<const int> items, const std::vector<int>::iterator count) {
	const int x = items.front();
	if (items.size() == 1) {
		*count = S / x;
		co_yield {};
	} else {
		for (int i = S / x; i >= 0; --i) {
			*count = i;
			co_yield helper(S - i * x, items.subspan(1), std::next(count));
		}
	}
}
} // namespace detail

// Coroutine, employing the classic, recursive pattern generation technique.
cppcoro::generator<std::span<const int>> cuttingStockPatternsRec(const int S, std::span<const int> items) {
	std::vector<int> counts(items.size(), 0); // Number of pieces of each item length in a given pattern
	// The helper signals each time a pattern is completed -> ignore its result & yield the pattern itself
	for (auto _ : detail::helper(S, items, counts.begin())) {
		co_yield counts;
	}
}

// An equivalent iterative process that keeps the current recursion "state"
// in just three integers, resulting in much more efficient pattern generation
// - no recursive coroutines & coroutine frame allocations, nothing.
cppcoro::generator<std::span<const int>> cuttingStockPatterns(const int S, std::span<const int> items) {
	const int n = int(items.size());
	std::vector<int> counts(n, 0); // Number of pieces of each item length in a given pattern
	int idx = -1; // Index of the current counter to be modified
	int S1 = S; // Remaining sum (invariant: S1 == S - sumproduct(xs,cnts))
	bool unwinding = false; // Whether we are currently back-tracking, i.e. unwinding
	                        // the recursion until we reach a non-zero counter (if any)
	while (true) {
		if (unwinding) {
			if (idx == -1) {
				break; // We've looped through all patterns, nothing more to do
			} else if (counts[idx] == 0) {
				--idx;
			} else { // Found a counter to decrement, prepare for next recursion step
				--counts[idx];
				S1 += items[idx];
				unwinding = false;
			}
		} else {
			if (idx == n - 1) { // Recursion base - we've completed a pattern & will return it before unwinding back
				co_yield counts;
				S1 += counts[idx] * items[idx];
				counts[idx] = 0; // Prepare for unwinding
				unwinding = true;
			} else {
				++idx;
				counts[idx] = S1 / items[idx];
				S1 -= counts[idx] * items[idx];
			}
		}
	}
}

/* Example usage:
	const int n = 20;
	const std::vector<int> items{ 9,7,5 };
	std::cout << "Cutting stock patterns:\n";
	for (const auto& pat : cuttingStockPatterns(n, items)) {
		for (const int x : pat) { std::cout << x << ' '; }
		// Compute whatever we need with the current pattern, f.e. the waste
		const int waste = n - std::inner_product(items.begin(), items.end(), pat.begin(), 0);
		std::cout << "(waste=" << waste << ")\n";
	}
*/
