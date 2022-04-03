#pragma once

// vasserts are ordinarily controlled by the configuration, but it can be overridden
#if !defined(NDEBUG)
#define VASSERT_ENABLED
#endif // !NDEBUG

#ifdef VASSERT_ENABLED
// Include the full assert definition regardless of configuration
#ifdef NDEBUG
// To enable vassert, we have to undefine NDEBUG before including <cassert>
#undef NDEBUG
#include <cassert>
#define NDEBUG
#else
#include <cassert>
#endif // NDEBUG

// This allows capturing expression with commas, without them being recognized
// as additional macro arguments
#define vassert(...) assert((__VA_ARGS__))
constexpr bool vassertEnabled = true;
#else
#define vassert(...) ((void)0)
constexpr bool vassertEnabled = false;
#endif // VASSERT_ENABLED
