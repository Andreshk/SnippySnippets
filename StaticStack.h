#pragma once
#include <cstdint> // size_t
#include <array>

// A simple stack w/ fixed maximum size & an interface, roughly matching the dynamic std::stack
template <typename T, size_t N>
class StaticStack {
	std::array<T, N> data;
	int topIdx;
public:
	StaticStack() noexcept : data{}, topIdx{ -1 } {}
	void push(const T& val) noexcept { data[++topIdx] = val; }
	T& top() noexcept { return data[topIdx]; }
	void pop() noexcept { data[topIdx--] = T{}; } // At least deinitialize to avoid potential bugs & improve visualization
	bool empty() const noexcept { return (topIdx < 0); }
	size_t size() const noexcept { return (topIdx + 1); }
};
