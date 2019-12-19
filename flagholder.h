#pragma once
#include <type_traits> // std::{is_enum_v,underlying_type_t,std_enable_if_t}

/* We often have "flag" types - a type, used to indicate a subset of a given (usually small) set of options (flags)
 * These types are typically represented as integers, in which every bit corresponds
 * to a single flag, and multiple flags are combined with bitwise disjunction.
 * The flags themselves are often declared as pure integers, too, or worse - a macro
 * for an integral value. This is incredibly unsafe - flags and raw integers can be
 * used interchangeably, even in the place of other sets of flags.
 * 
 * The solution proposed here enforces full type safety at no performance cost. The only
 * requirement left unenforced is that the flags' values do not have overlapping bits
 * (for example, they can all be distinct powers of 2).
 */

// As a first step, the flags can be defined as a scoped enum (with proper values).
enum class RT : unsigned {
    Foo = (1u << 0),
    Bar = (1u << 1),
    Baz = (1u << 2),
    Gaz = (1u << 3),
};

// Another, separate set of flags - this is now a different type
enum class PLS : unsigned {
    Foo = (1u << 0),
    Bar = (1u << 1),
    Baz = (1u << 2),
};

// A flag "holder" struct keeps the bitwise | of separate enum values as a pure integer.
// It is expected (and important) that all operations with this struct are as fast as
// operations on pure integers. Everything is noexcept and can be constexpr, but the
// specifiers are omitted for simplicity.
template <typename FlagT>
struct FlagHolder {
    static_assert(std::is_enum_v<FlagT>); // Preferred to SFINAE
    // We can use the proper underlying type - or simply define this as unsigned or whatever.
    using UnderlyingT = std::underlying_type_t<FlagT>;
private:
    UnderlyingT flags; // The only member - a simple integer.
    FlagHolder(const UnderlyingT flags) : flags(flags) {}
public:
    // No bits set by default
    FlagHolder() : FlagHolder(0) {}
    // Intentionally implicit converting constructor - this allows passing
    // single enum values to functions, expecting a FlagHolder struct.
    FlagHolder(const FlagT flag) : FlagHolder(static_cast<UnderlyingT>(flag)) {}

    // All operators are defined as usual (this is a minimal example)
    // Also works for single enum values, due to the implicit conversion.
    FlagHolder operator|(const FlagHolder &other) const {
    	return FlagHolder(flags | other.flags);
    }
};

// The key to this type-safety jump now is the ability to overload operators for the enum values,
// in which the result is converted to a FlagHolder for that enum - it may not be a valid enum value (!)
// Now all functions that work with flags of a certain type can take a FlagHolder argument instead,
// and no matter how many flags are passed at the call-site, the result will be a proper, well-behaved struct.
template <typename FlagT, typename = std::enable_if_t<std::is_enum_v<FlagT>>>
FlagHolder<FlagT> operator|(const FlagT& f1, const FlagT& f2) {
    return (FlagHolder(f1) | FlagHolder(f2));
}

// Giving a separate name to each FlagHolder specialization can improve usability,
// while keeping the user code as readable and easily understandable as before.
using RayFlags = FlagHolder<RT>;
using PlsFlags = FlagHolder<PLS>;

/* Using the definitions above, this would be an example of user code. Note its
 * simplicity and readability - there are no "catches", and the complexity is only internal.
 *
 *  void useRayFlags(const RayFlags&);
 *  
 *  useRayFlags(RT::Foo); -> implicit conversion to FlagHolder
 *  useRayFlags(RT::Foo | RT::Baz);
 *  useRayFlags(RT::Foo | RT::Baz | RT::Gaz); -> the left | is the external, enum-converting operator,
 *                                               and the right one is the member operator.
 */
