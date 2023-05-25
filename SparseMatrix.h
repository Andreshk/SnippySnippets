#pragma once
#include <span>
#include <vector>
#include <ranges> // std::views::{iota,transform}, std::ranges::to
#include <concepts> // std::integral
#include <algorithm> // std::ranges::{find_if,none_of}
#include <cassert>

template <std::integral T>
class SparseMatrix {
	std::vector<bool> marked;
	std::vector<int> offsets;
	std::vector<T> data;
	int cols;
	int nil;
public:
	SparseMatrix(std::span<const T> m, const int cols, const T nil = 0) : marked(m.size(), false), offsets(m.size() / cols, 0), cols(cols), nil(nil) {
		const int rows = int(offsets.size());
		assert(m.size() == rows * cols);
		// Add rows in order of their # of empty values
		auto counts = std::views::iota(0, rows)
			| std::views::transform([&](int i) { return std::make_pair(i, int(std::ranges::count(m.subspan(i * cols, cols), nil))); })
			| std::ranges::to<std::vector>();
		std::ranges::sort(counts, std::less<>{}, &std::pair<int, int>::second);
		for (int prev = 0; const auto [row, _] : counts) {
			// Find the smallest offset, starting from the previous, such that
			// the non-zero values of this row fit with the non-zero values already in data.
			const int off = *std::ranges::find_if(std::views::iota(prev), [&](int off) {
				return std::ranges::none_of(std::views::iota(0, std::min(cols, int(data.size()) - off)), [&](int j) {
					return m[row * cols + j] != nil && data[off + j] != nil;
				});
			});
			assert(off <= prev + cols); // In the worst case, the row will just be appended
			// Resize up to fit the new row if needed
			if (off + cols > data.size()) {
				data.resize(off + cols, nil);
			}
			for (int j = 0; j < cols; ++j) {
				if (m[row * cols + j] != nil) {
					data[off + j] = m[row * cols + j];
					marked[row * cols + j] = true;
				}
			}
			offsets[row] = off;
			prev = off;
		}
	}
	const T& at(const int i, const int j) const {
		assert(i >= 0 && i < offsets.size() && j >= 0 && j < cols);
		return (marked[i * cols + j] ? data[offsets[i] + j] : nil);
	}
};
