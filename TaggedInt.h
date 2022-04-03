#pragma once
#include <concepts>    // std::integral
#include <type_traits> // std::is_enum_v
#include <iostream>    // stream input&output
#include <fmt/core.h>  // fmtlib formatted output (analogous to std::format)

// Define this to disable tagged integers & make them all a type synonym of int.
// Note: can be defined externally, f.e. before including this file or from command-line/CMake.
//#define DISABLE_TAGGED_INTS

#ifdef DISABLE_TAGGED_INTS
template <auto Tag, std::integral Idx = int>
using Int = Idx;
#else

// Represents an integer of type Idx, "tagged" with an enum value for clarity (can be scoped/unscoped).
// Integers, tagged with different values, are therefore different types and cannot be converted implicitly
// from one to another (or to/from raw integers). Explicit conversions are also strongly discouraged, but are
// available for compatibility/interfacing. Suitable for general usage, where it behaves like a regular integer.
// Can also be used for indexing of arrays that are in turn "tagged" via their index type and do not allow mixing index semantics.
// WARNING: std::iota_view of these will not be random_access - use it in standard algorithms with caution (!)
template <auto Tag, std::integral Idx = int>
	requires std::is_enum_v<decltype(Tag)>
class Int {
	Idx i;
public:
	Int() = default;
	explicit Int(Idx i) : i{ i } {}
	explicit operator Idx() const { return i; }

	friend Int operator+(Int lhs, Int rhs) { return Int{ lhs.i + rhs.i }; }
	friend Int operator-(Int lhs, Int rhs) { return Int{ lhs.i - rhs.i }; }
	friend Int operator*(Int lhs, Int rhs) { return Int{ lhs.i * rhs.i }; }
	friend Int operator/(Int lhs, Int rhs) { return Int{ lhs.i / rhs.i }; }
	friend Int operator%(Int lhs, Int rhs) { return Int{ lhs.i % rhs.i }; }
	friend Int operator^(Int lhs, Int rhs) { return Int{ lhs.i ^ rhs.i }; }
	friend Int operator&(Int lhs, Int rhs) { return Int{ lhs.i & rhs.i }; }
	friend Int operator|(Int lhs, Int rhs) { return Int{ lhs.i | rhs.i }; }
	Int operator~() const { return Int{ ~i }; }
	explicit operator bool() const { return bool(i); }
	friend constexpr bool operator==(const Int& lhs, const Int& rhs) { return (lhs.i == rhs.i); }
	friend constexpr auto operator<=>(const Int& lhs, const Int& rhs) { return (lhs.i <=> rhs.i); }
	Int& operator+=(Int rhs) { i += rhs.i; return *this; }
	Int& operator-=(Int rhs) { i -= rhs.i; return *this; }
	Int& operator*=(Int rhs) { i *= rhs.i; return *this; }
	Int& operator/=(Int rhs) { i /= rhs.i; return *this; }
	Int& operator%=(Int rhs) { i %= rhs.i; return *this; }
	Int& operator^=(Int rhs) { i ^= rhs.i; return *this; }
	Int& operator&=(Int rhs) { i &= rhs.i; return *this; }
	Int& operator|=(Int rhs) { i |= rhs.i; return *this; }
	Int operator>>(int shift) const { return Int{ i >> shift }; }
	Int operator<<(int shift) const { return Int{ i << shift }; }
	Int& operator>>=(int shift) { i >>= shift; return *this; }
	Int& operator<<=(int shift) { i <<= shift; return *this; }
	Int& operator++() { ++i; return *this; }
	Int& operator--() { --i; return *this; }
	Int operator++(int) const { return Int{ i++ }; }
	Int operator--(int) const { return Int{ i-- }; }
	friend std::istream& operator>>(std::istream& is,       Int& rhs) { return (is >> rhs.i); }
	friend std::ostream& operator<<(std::ostream& os, const Int& rhs) { return (os << rhs.i); }

	// Convenience operators that take a tagged and a raw int and promote the raw.
	// This is all better than allowing implicit conversions from/to ints.
	friend Int operator+(Int lhs, Idx rhs) { return Int{ lhs.i + rhs }; }
	friend Int operator+(Idx lhs, Int rhs) { return Int{ lhs + rhs.i }; }
	friend Int operator-(Int lhs, Idx rhs) { return Int{ lhs.i - rhs }; }
	friend Int operator-(Idx lhs, Int rhs) { return Int{ lhs - rhs.i }; }
	friend Int operator*(Int lhs, Idx rhs) { return Int{ lhs.i * rhs }; }
	friend Int operator*(Idx lhs, Int rhs) { return Int{ lhs * rhs.i }; }
	friend Int operator/(Int lhs, Idx rhs) { return Int{ lhs.i / rhs }; }
	friend Int operator/(Idx lhs, Int rhs) { return Int{ lhs / rhs.i }; }
	friend Int operator%(Int lhs, Idx rhs) { return Int{ lhs.i % rhs }; }
	friend Int operator%(Idx lhs, Int rhs) { return Int{ lhs % rhs.i }; }
	Int& operator+=(Idx rhs) { i += rhs; return *this; }
	Int& operator-=(Idx rhs) { i -= rhs; return *this; }
	Int& operator*=(Idx rhs) { i *= rhs; return *this; }
	Int& operator/=(Idx rhs) { i /= rhs; return *this; }
	Int& operator%=(Idx rhs) { i %= rhs; return *this; }
	friend bool operator==(const Idx& lhs, const Int& rhs) { return (lhs == rhs.i); }
	friend bool operator==(const Int& lhs, const Idx& rhs) { return (lhs.i == rhs); }
	friend auto operator<=>(const Idx& lhs, const Int& rhs) { return (lhs <=> rhs.i); }
	friend auto operator<=>(const Int& lhs, const Idx& rhs) { return (lhs.i <=> rhs); }
	
	// Now that we have all promoting operators, we can safely
	// define this AND keep our type-safe operator-.
	// We can now, for example, use tagged ints in std::ranges::iota_view.
	using difference_type = Idx;

	// Allow for formatted output via fmtlib, supporting
	// all the ways a regular int can be formatted.
	friend struct fmt::formatter<Int>;
};

// to-do: replace with std::formatter when C++23 is enabled
template <auto Tag, std::integral Idx>
class fmt::formatter<Int<Tag, Idx>> {
	fmt::formatter<Idx> valueFormatter;
public:
	template <typename ParseContext>
	constexpr auto parse(ParseContext& ctx) {
		return valueFormatter.parse(ctx);
	}
	template <typename FormatContext>
	auto format(const Int<Tag, Idx>& i, FormatContext& ctx) const {
		return valueFormatter.format(i.i, ctx);
	}
};

#endif // !DISABLE_TAGGED_INTS
