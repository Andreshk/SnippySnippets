#include <fmt/core.h>
#include <random> // std::{uniform_random_bit_generator,uniform_int_distribution}
#include <vector>
#include <cmath> // std::floor
#include <cassert>
import SplitMix;
import Xoshiro;

#include <ranges>
namespace ranges { // not std (!)
template <typename Rng, typename Proj = std::identity>
auto sum(Rng&& rng, Proj proj = {}) {
	std::decay_t<std::invoke_result_t<Proj, std::ranges::range_reference_t<Rng>>> res{}; // as per rangev3's fold
	for (auto&& x : rng) { res += std::invoke(proj, std::forward<decltype(x)>(x)); }
	return res;
}
}

template <typename Gen>
concept RandomFloatingGenerator = requires (Gen& g) { { g() } -> std::floating_point; };

template <typename Gen>
	requires (std::uniform_random_bit_generator<Gen> || RandomFloatingGenerator<Gen>)
void test() {
	const int n = 1000000;
	const int k = 100;
	Gen gen{};
	[[maybe_unused]] std::uniform_int_distribution distr(0, k - 1);
	std::vector<int> counts(k, 0);
	for (int i = 0; i < n; ++i) {
		if constexpr (std::uniform_random_bit_generator<Gen>) {
			++counts[distr(gen)];
		} else {
			++counts[int(std::floor(gen() * k))];
		}
	}
	// The result should approach an uniform distibution over [0;k), so compare with that
	std::vector<double> probs(k, 0.);
	for (int i = 0; i < k; ++i) {
		probs[i] = double(counts[i]) / n; // Average 1/k
	}
	assert(std::abs(ranges::sum(probs) - 1) < 1e-6f);
	const double mean = (double(k) - 1) / 2;
	const double var = ranges::sum(std::views::iota(0, k), [&](int i) { return probs[i] * (i - mean) * (i - mean); });
	const double expVar = (double(k) * k - 1) / 12;
	fmt::print("var={:.6} exp={:.6} diff={:+.2}%\n", var, expVar, 100 * (var - expVar) / expVar);
}

int main() {
	test<SplitMix64>();
	test<SplitMix64F>();
	test<Xoshiro128>();
	test<Xoshiro256>();
	test<Xoshiro128P>();
	test<Xoshiro256P>();
	test<Xoshiro128F>();
	test<Xoshiro256F>();
}
