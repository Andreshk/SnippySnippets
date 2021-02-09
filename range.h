#pragma once
#include <concepts>
#include <iterator>
#include <type_traits>
#include <ranges>

// Represents an integral half-closed range [from;to), that can be iterated as a
// random-access container (even models the std::ranges::random_access_range concept).
// Serves as a homebrew, simpler version of std::ranges::views::iota.
// Everything can be constexpr and is noexcept - modifiers are omitted for brevity.
template <std::integral T> // Long live concepts
class range {
	// Range boundaries
	T from, to;
public:
	// Invalid ranges are clamped to empty ones
	range(T from, T to) : from{ from }, to{ (to > from ? to : from) } {
		// Cannot place this in the class body (the class is considered incomplete there),
		// but can place it any member function - so we go for the one, always instantiated :)
		static_assert(std::ranges::random_access_range<range>);
	}
	// Unlike iota, range(n) is equivalent to range(0,n)
	explicit range(T to) : range(0, to) {}
	// Definition below
	class iterator;
	// Standard container interface. Read-only, since it doesn't actually contain anything.
	iterator begin() const { return { from }; }
	iterator end() const { return { to }; }
	using const_iterator = iterator;
	const_iterator cbegin() const { return begin(); }
	const_iterator cend() const { return end(); }
	T size() const { return (to - from); }
	bool empty() const { return (from == to); }

	// Full-fledged random access iterator (models the std::random_access_iterator concept).
	class iterator {
		// The current value in the range, "pointed to" by this iterator
		T curr;
		// Private constructor - only a range knows how to constructs iterators to itself.
		friend range;
		iterator(T curr) : curr{ curr } {}
	public:
		// std::random_access_iterator indirectly requires std::default_initializable,
		// meaning we have to provide a visible, nonsense default constructor for the iterator.
		// (the requirements chain is random_access_iterator -> bidirectional_iterator
		// -> forward_iterator -> incrementable -> weakly_incrementable -> default_initializable)
		iterator() : curr{ 0 } {}
		// Iterator value types should be non-const even for read-only containers
		using value_type = T; 
		using pointer    = const T*;
		using reference  = const T&;
		using difference_type   = std::make_signed_t<T>;
		using iterator_category = std::random_access_iterator_tag;
		// All other methods, required by the std::random_access_iterator concept.
		reference operator*() const { return curr; }
		iterator operator+(const difference_type n) const { return { curr + n }; }
		iterator operator-(const difference_type n) const { return { curr - n }; }
		iterator& operator+=(const difference_type n) { curr += n; return *this; }
		iterator& operator-=(const difference_type n) { curr -= n; return *this; }
		reference operator[](const difference_type n) const { return *(*this + n); }
		difference_type operator-(const iterator& other) const { return (curr - other.curr); }
		friend iterator operator+(const difference_type n, const iterator& it) { return (it + n); }
		iterator& operator++() { ++curr; return *this; }
		iterator& operator--() { --curr; return *this; }
		iterator operator++(int) { auto copy = *this; ++(*this); return copy; }
		iterator operator--(int) { auto copy = *this; --(*this); return copy; }
		// Defaulted 3-way comparison operator removes some boilerplate at least
		auto operator<=>(const iterator& other) const = default;
	};
	// We can now add this here
	static_assert(std::random_access_iterator<iterator>);
};
