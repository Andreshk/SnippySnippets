#pragma once
#ifdef DEBUG_RANDOM
#include <iostream>
#endif // DEBUG_RANDOM
#include <chrono>    // for seeding the random generator
#include <random>
#include <ranges>    // std::ranges::random_access_range
#include <cmath>     // std::floor
#include <algorithm> // std::copy_n

// Controls the precision of floating-point calculations.
using Real = float;

// The engine, used to generate all random numbers required by the sampling
// functions. Can print out its seed on construction to aid in debugging.
inline std::mt19937_64 eng([]() {
    const auto seed = std::chrono::steady_clock::now().time_since_epoch().count();
#ifdef DEBUG_RANDOM
    std::cout << "Using seed = " << seed << '\n';
#endif // DEBUG_RANDOM
    return seed;
}());

// Returns a random floating-point number in [0,1)
Real random() {
    static const auto distr = std::uniform_real_distribution<Real>();
    return distr(eng);
}

// Implements the naive, linear Algorithm R for reservoir sampling
// from a given range to another, smaller preallocated range.
// For the InputRange, use a vector, or std::ranges::views::iota,
// or a range from range.h in the same repo :)
template <std::ranges::random_access_range InputRange,
          std::ranges::random_access_range OutputRange>
void reservoirSample(const InputRange& xs, OutputRange& samples) {
    const auto k = samples.end() - samples.begin();
    if (k == 0) {
        return;
    }
    using Iter = typename InputRange::const_iterator;
    using T = typename std::iterator_traits<Iter>::value_type;
    const Iter from = xs.begin(), to = xs.end();
    // First k values are always selected
    std::copy_n(from, k, samples.begin());
    for (auto it = from + k; it != to; ++it) {
        const auto i = it - from + 1; // 1-based index of the inserted value
        const Real prob = Real(k) / i;
        const Real x = random();
        if (x < prob) {
            // Interpolate from [0;prob) to [0;k) to randomly select the index of a value to overwrite
            const int idx = int(std::floor(x * k / prob));
            samples[idx] = *it;
        }
    }
}

// Faster reservoir sampling via the O(k*log(n/k)) Algorithm L, which is
// optimal for the task. It works by calculating the expected gaps between
// each successfully added value and skipping over the remaining values.
// It also draws only 3 random numbers for each of the O(k*log(n/k)) values
// added, instead of 1 for each of the n values that the linear sampler does.
// Again, samples from a given range to another, smaller preallocate one.
template <std::ranges::random_access_range InputRange,
          std::ranges::random_access_range OutputRange>
void fastReservoirSample(const InputRange& xs, OutputRange& samples) {
    const auto k = samples.end() - samples.begin();
    if (k == 0) {
        return;
    }
    using Iter = typename InputRange::const_iterator;
    using T = typename std::iterator_traits<Iter>::value_type;
    const Iter from = xs.begin(), to = xs.end();
    // First k values are always selected
    std::copy_n(from, k, samples.begin());
    // Iterator to the next value to be directly added in the samples array.
    // -1 so that we don't miss the value with index k when incrementing later (!)
    Iter it = from + k - 1;
    Real w = 1;
    // Calculates the gap to the next value to be successfully added
    auto advance = [k](Iter& it, Real& w) {
        w *= std::exp(std::log(random()) / k);
        it += int(std::floor(std::log(random()) / std::log(1 - w))) + 1;
    };
    advance(it, w);
    // Performs an iteration per added value - O(k*log(n/k))
    while (it < to) {
        const int idx = int(std::floor(random() * k));
        samples[idx] = *it;
        advance(it, w);
    }
}
