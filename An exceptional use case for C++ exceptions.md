# An exceptional use case for C++ exceptions

The use of exceptions in production C++ has been a hot & widely debated topic for a long time. There is, however, one somewhat unrelated scenario, in which `throw`-ing an exception is _the only_ solution.

**One-liner:** the only way to validate an intermediate compile-time value is simply to use a `throw` expression in a (hopefully unevaluated) conditional branch.

## Problem

`constexpr` functions have two main applications: calculating certain useful values during compilation time, and verifying some crucial property (an invariant), suspending compilation with a `static_assert` if it doesn't hold. The thing is, `static_assert` can only be used to validate the final return value of some (maybe complex) `constexpr` function. What about all the intermediate values? Or the input to some `constexpr` function?

Let `is_even()` be the super-important property we want to hold for `foo()`-s input:
```c++
constexpr bool is_even(int x) { return (x % 2 == 0); }
constexpr int foo(int x) {
    // if x is not even, suspend compilation
    // otherwise, do whatever work is needed
    return x * 2;
}

static_assert(foo(4) == 8); // forcing a compile-time call to foo()
```

Both `is_even()` and `foo()` can be much more complex functions, especially since C++14, perhaps called at a deeper call-stack level. The logical choice would be `static_assert(is_even(x))` - but `is_even(x)` is not a compile-time constant (obviously) and cannot be used in a `static_assert`. However, it's a value that _can be_ calculated during compile-time for a given `x`. So, how can we recognize and validate these values as well?

#### How else can a compile-time check be performed, blocking compilation on failure?

## Solution(s)
One approach would be to validate the input before the call:
```c++
constexpr int x = 4;
static_assert(is_even(x));
static_assert(foo(x) == 8);
```
This is obviously a bad idea if `foo()` is called more than once; it's a generally bad design choice and again does not work if `foo()` is being called by another `constexpr` function.

One might look at the different restrictions to `constexpr` functions and try to break compilation if the input property isn't held:
* declaring a label or using a `goto`
* defining a non-initialized local variable
* defining a static variable
* defining a variable of non-literal type
* an asm declaration
* a try-block

Either of the lines below causes a compilation error in the latest versions of MSVC, ICC, GCC and Clang _even when unevaluated_, all with the general standard-abiding message of "such a statement is not allowed in a constexpr function":
```c++
if (!is_even(x)) {
    goto test;
    test:
    int y;
    static int y = 5;
    struct B { virtual ~B(); } b{};
    static_assert(false);
}
```
`static_assert(false)` is a compilation-blocker even if placed in an `if (false) { ... }` block. `if constexpr` cannot be used for the same reason as `static_assert`. There is one exception (pun intended) - using a `throw` expression:
```c++
constexpr bool is_even(int x) { return (x % 2 == 0); }
constexpr int foo(int x) {
    if (!is_even(x)) { throw "oops"; }
    return x * 2;
}
static_assert(foo(4) == 8);
```
Just like all the other examples above, `throw` is also forbidden in a `constexpr` function. However, the compiler is _allowed to_ (and **will**) ignore a `throw` expression if it is not evaluated during an actual compile-time computation. So, when `foo(4)` is called, `is_even(4)` is evaluated to true, the `throw` expression is _skipped_, and the code above compiles successfully. Trying to evaluate `static_assert(foo(5))` will try to evaluate the `throw` expression, now causing a compiler error - exactly the desired behaviour.

Of course, this trick has been known since the early C++11 days of single-statement `constexpr` functions. Still, it is an interesting, simple and straight-forward use case of "compile-time" exceptions, when `static_assert` is too strong, but the check _can be_ performed during compilation.

All behaviours, described above, can be verified easily [here](https://godbolt.org/g/W6QDdb).
