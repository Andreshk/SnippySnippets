#include "Generator.h"
#include <vector>

// Coroutine, generating all possible patterns for a cutting stock problem.
// This, of course, is a minor step in the process of solving this problem.
// Each pattern is identified by the number of occurences of each item.
// Note: coroutine parameters must be passed by value (& this should've been an array view)
Generator<std::vector<int>> cuttingStockPatternsGenerator(const int S, const std::vector<int> items) {
	const int n = int(items.size());
	std::vector<int> counts(n, 0); // Number of pieces of each item length in a given pattern
	// This simple, inherently recursive process is transformed into an iterative one that keeps the
	// recursion "state" - current index & whether we are currently back-tracking - at any point in time.
	int idx = -1; // Index of the current counter to be modified
	int S1 = S; // Remaining sum (invariant: S1 == S - sumproduct(xs,cnts))
	bool unwinding = false; // Whether we are currently unwinding the recursion until we reach a non-zero counter, if any
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
			if (idx == n - 1) { // Recursion base - we've completed a pattern
				// Return the current pattern to the callee before unwinding back
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
	Generator<std::vector<int>> patternsGen = cuttingStockPatternsGenerator(20, { 9,7,5 });
	std::cout << "Cutting stock patterns:\n";
	while (patternsGen.next()) {
		const std::vector<int>& pat = *patternsGen;
		for (const int x : pat) { std::cout << x << ' '; }
		// Compute whatever we need with the current pattern, f.e. the waste
		const int waste = n - std::inner_product(items.begin(), items.end(), pat.begin(), 0);
		std::cout << "(waste=" << waste << ")\n";
	}
*/


