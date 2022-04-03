#pragma once
#include <concepts> // std::integral
#include <ranges>   // std::ranges::contiguous_range
#include <memory>   // std::shared_ptr
#include <utility>  // std::exchange
#include <type_traits>
#include <initializer_list>
#include "TaggedArrayIterator.h"
#include "TaggedInt.h"
#include "vassert.h"

namespace {
	template <typename T, bool isEnum> struct ToUnderlyingType_;
	template <typename T>
	struct ToUnderlyingType_<T, true> { using type = std::underlying_type_t<T>; };
	template <std::integral T>
	struct ToUnderlyingType_<T, false> { using type = T; };
#ifndef DISABLE_TAGGED_INTS
	template <auto Tag, std::integral Idx>
	struct ToUnderlyingType_<Int<Tag, Idx>, false> { using type = Idx; };
#endif // !DISABLE_TAGGED_INTS
	// Helper meta-function to determine the underlying integral type for a type to be used for indexing.
	// We cannot use std::conditional_t<std::is_enum_v<T>, std::underlying_type_t<T>, T>
	// since this always (strictly) evaluates both branches, causing an error for non-enum T.
	template <typename T>
	using ToUnderlyingType = typename ToUnderlyingType_<T, std::is_enum_v<T>>::type;
}

// Simple dynamic array, parameterized by index type to avoid using one type
// of index in the place of another. Supports pure integers, enum types and
// (most importantly) "tagged" integers (see TaggedInt.h).
template <typename T, typename Index = int>
	requires std::integral<ToUnderlyingType<Index>>
class DynamicArray {
	using _Index = ToUnderlyingType<Index>; // This is now a standard, integral type

	T* data;
	_Index numElements;
public:
	DynamicArray() : data{ nullptr }, numElements{ 0 } {}
	explicit DynamicArray(const Index numElements) : data{ new T[_Index(numElements)] }, numElements{ _Index(numElements) } {};
	template <typename U>
		requires std::constructible_from<T, const U&>
	explicit DynamicArray(const std::initializer_list<U>& il)
		: DynamicArray(static_cast<Index>(il.size()))
	{
		for (_Index i = 0; i < numElements; ++i) {
			data[i] = T(il.begin()[i]);
		}
	}
	// Move-ctor/assignment should be explicitly marked as noexcept
	DynamicArray(DynamicArray&& other) noexcept : data{ std::exchange(other.data, nullptr) }, numElements{ std::exchange(other.numElements, 0) } {}
	DynamicArray& operator=(DynamicArray&& other) noexcept {
		if (this != &other) {
			this->~DynamicArray();
			new (this) DynamicArray{ std::move(other) };
		}
		return *this;
	}
	// Copy-ctor and copy-assignment are now implicitly deleted
	~DynamicArray() {
		// Cannot place this is the class body (class is considered incomplete there),
		// so we place it here in a method that's guaranteed to be instantiated :)
		static_assert(std::ranges::contiguous_range<DynamicArray>);
		if (data) {
			delete[] data;
			data = nullptr;
			numElements = 0;
		}
	}

	T& operator[](const Index idx) {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	const T& operator[](const Index idx) const {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	Index size() const { return Index(numElements); }
	bool empty() const { return (numElements == 0); }

	// Iterator boilerplate has been reduced via the helper ArrayIterator class
	using iterator = ArrayIterator<T, DynamicArray>;
	iterator begin() { return { data, this }; }
	iterator end() { return { data + numElements, this }; }
	using const_iterator = ArrayIterator<const T, const DynamicArray>;
	const_iterator begin() const { return { data, this }; }
	const_iterator end() const { return { data + numElements, this }; }
	const_iterator cbegin() const { return begin(); }
	const_iterator cend() const { return end(); }
	static_assert(std::contiguous_iterator<iterator>);
	static_assert(std::contiguous_iterator<const_iterator>);
};

// Ref-counted dynamic array, also parameterized by index
template <typename T, typename Index = int>
	requires std::integral<ToUnderlyingType<Index>>
class SharedArray {
	using _Index = ToUnderlyingType<Index>; // This is now a standard, integral type

	std::shared_ptr<T[]> data;
	_Index numElements;
public:
	SharedArray() : data{}, numElements{ 0 } {}
	explicit SharedArray(const Index numElements) : data{ std::make_shared<T[]>(size_t(numElements)) }, numElements{ _Index(numElements) } {}
	// Move-ctor/assignment should be explicitly marked as noexcept
	// They are defined to guarantee that moved-from arrays will have numElements == 0 (otherwise moved-from ints are garbage)
	SharedArray(SharedArray&& other) noexcept : data{ std::move(other.data) }, numElements{ std::exchange(other.numElements, 0) } {}
	SharedArray& operator=(SharedArray&& other) noexcept {
		if (this != &other) {
			this->~SharedArray();
			new (this) SharedArray{ std::move(other) };
		}
		return *this;
	}
	// Copy-ctor/assignment can now be defaulted
	SharedArray(const SharedArray&) = default;
	SharedArray& operator=(const SharedArray&) = default;
	~SharedArray() {
		// Cannot place this is the class body (class is considered incomplete there),
		// so we place it here in a method that's guaranteed to be instantiated :)
		static_assert(std::ranges::contiguous_range<SharedArray>);
		data.reset();
		numElements = 0; // Also leave size 0 for destructed arrays
	}

	T& operator[](const Index idx) {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	const T& operator[](const Index idx) const {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	Index size() const { return Index(numElements); }
	bool empty() const { return (numElements == 0); }

	// Iterator boilerplate has been reduced via the helper ArrayIterator class
	using iterator = ArrayIterator<T, SharedArray>;
	iterator begin() { return { data.get(), this }; }
	iterator end() { return { data.get() + numElements, this }; }
	using const_iterator = ArrayIterator<const T, const SharedArray>;
	const_iterator begin() const { return { data.get(), this }; }
	const_iterator end() const { return { data.get() + numElements, this }; }
	const_iterator cbegin() const { return begin(); }
	const_iterator cend() const { return end(); }
	static_assert(std::contiguous_iterator<iterator>);
	static_assert(std::contiguous_iterator<const_iterator>);
};

// Simple static array wrapper. Captures the type of the
// size value again to avoid using the wrong index type.
template <typename T, auto N>
	requires std::integral<ToUnderlyingType<decltype(N)>>
class StaticArray {
public:
	using Index = decltype(N);
private:
	using _Index = ToUnderlyingType<Index>; // This is now a standard, integral type

	T data[_Index(N)];
public:
	T& operator[](const Index idx) {
		vassert(_Index(idx) >= 0 && _Index(idx) < _Index(N));
		return data[_Index(idx)];
	}
	const T& operator[](const Index idx) const {
		vassert(_Index(idx) >= 0 && _Index(idx) < _Index(N));
		return data[_Index(idx)];
	}
	~StaticArray() {
		// Cannot place this is the class body (class is considered incomplete there),
		// so we place it here in a method that's guaranteed to be instantiated :)
		static_assert(std::ranges::contiguous_range<StaticArray>);
	}
	Index size() const { return N; }
	bool empty() const { return (size() == 0); }

	// Iterator boilerplate has been reduced via the helper ArrayIterator class
	using iterator = ArrayIterator<T, StaticArray>;
	iterator begin() { return { data, this }; }
	iterator end() { return { data + _Index(N), this }; }
	using const_iterator = ArrayIterator<const T, const StaticArray>;
	const_iterator begin() const { return { data, this }; }
	const_iterator end() const { return { data + _Index(N), this }; }
	const_iterator cbegin() const { return begin(); }
	const_iterator cend() const { return end(); }
	static_assert(std::contiguous_iterator<iterator>);
	static_assert(std::contiguous_iterator<const_iterator>);
};

// A non-owning view of an array, also parameterized by index type.
template <typename T, typename Index = int>
	requires std::integral<ToUnderlyingType<Index>>
class ArrayView {
	using _Index = ToUnderlyingType<Index>; // This is now a standard, integral type

	T* data;
	_Index numElements;
public:
	ArrayView() : data{ nullptr }, numElements{ 0 } {}
	ArrayView(T* data, const Index numElements) : data{ data }, numElements{ _Index(numElements) } {}
	// Modifying array view constructor.
	// Intentionally non-explicit (converting to a non-owning view is not a problem).
	template <std::ranges::contiguous_range Range>
		requires !std::is_const_v<T> && std::is_same_v<T, std::ranges::range_value_t<Range>>
	ArrayView(Range& range)
		: data{ std::to_address(std::ranges::begin(range)) }
		, numElements{ _Index(std::ranges::size(range)) } {}
	// Non-modifying array view constructor. Also non-explicit.
	template <std::ranges::contiguous_range Range>
		requires std::is_const_v<T> && std::is_same_v<std::remove_const_t<T>, std::ranges::range_value_t<Range>>
	ArrayView(const Range& range) // Non-explicit, constructing an array view is not a problem
		: data{ std::to_address(std::ranges::begin(range)) }
		, numElements{ _Index(std::ranges::size(range)) } {}
	// Obtaining views to a temporary is forbidden - it will immediately become dangling.
	//template <std::ranges::contiguous_range Range>
	//ArrayView(Range&&) = delete;

	~ArrayView() {
		// Cannot place this is the class body (class is considered incomplete there),
		// so we place it here in a method that's guaranteed to be instantiated :)
		static_assert(std::ranges::contiguous_range<ArrayView>);
		data = nullptr;
		numElements = 0;
	}

	T& operator[](const Index idx) {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	const T& operator[](const Index idx) const {
		vassert(data && _Index(idx) >= 0 && _Index(idx) < numElements);
		return data[_Index(idx)];
	}
	Index size() const { return Index(numElements); }
	bool empty() const { return (numElements == 0); }
	// Intentionally implicit cast from modifying to non-modifying array view
	operator ArrayView<const T, Index>() const {
		return { data, Index(numElements) };
	}

	// Iterator boilerplate has been reduced via the helper ArrayIterator class
	using iterator = ArrayIterator<T, ArrayView>;
	iterator begin() { return { data, this }; }
	iterator end() { return { data + numElements, this }; }
	using const_iterator = ArrayIterator<const T, const ArrayView>;
	const_iterator begin() const { return { data, this }; }
	const_iterator end() const { return { data + numElements, this }; }
	const_iterator cbegin() const { return begin(); }
	const_iterator cend() const { return end(); }
	static_assert(std::contiguous_iterator<iterator>);
	static_assert(std::contiguous_iterator<const_iterator>);
};

#ifndef DEBUG_ITERATOR
// ArrayViews also satisfy the borrowed range concept, meaning that iterators
// to a view can outlive the view itself & will not become dangling.
// This, of course, holds only when the debug iterators are off -
// otherwise they contain a pointer to the container that created them.
template <typename T, typename Idx>
inline constexpr bool std::ranges::enable_borrowed_range<ArrayView<T, Idx>> = true;
#endif // !DEBUG_ITERATOR

