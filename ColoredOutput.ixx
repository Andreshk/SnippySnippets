/*
 * Simple mechanism for enabling colored output via the standard ANSI escape sequences for
 * Windows consoles that may not support it natively, by enabling virtual terminal support.
 * Also provides a small convenience enum class to represent the basic ANSI colors.
 *
 * This is highly preferred to using macros for the escape sequences - it avoids
 * printing garbage when colored output is not supported & eschews the miniscule
 * performance gain of the macros (when fused by the preprocessor into the string literals
 * to be outputted) for type safety, modularization & extensibility -
 * f.e. selective on-the-fly colored output pausing/resuming.
 *
 * For detailed info see https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#text-formatting
 * and for some nice examples: https://superuser.com/questions/413073/windows-console-with-ansi-colors-handling/1105718#1105718
 * and https://stackoverflow.com/a/33206814.
 */
export module ColoredOutput;

// Represents ANSI colors and some of their combinations.
export enum class Color {
    Default, // Resets the console color to the default one
    Bold,    // Not an actual color, but a modifier for subsequent outputs
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
};

// Converts a Color value to the corresponding ANSI escape sequence.
// Returns empty string if colored output is disabled.
// To be used mainly with printf-style functions, for example:
//   printf("%stest red%s", toString(Color::Red), toString(Color::Default));
export const char* toString(const Color);

// Outputs the ANSI escape sequence (obtained via toString() above) for the given Color,
// to to the given stream. Note that only std::cout and std::cerr are supported
// (others may not parse the sequences and print them as plain text), for example:
//   std::cout << Color::Red << "test red" << Color::Default;
import <iostream>;
export std::ostream& operator<<(std::ostream& os, const Color c);

// Enables formatting of Color-s via std::format (again via toString()), for example:
//   std::cout << std::format("{}test red{}\n", Color::Red, Color::Default);
// Note: cannot be used until https://developercommunity.visualstudio.com/t/Exported-template-specialization-from-mo/1449269
// is resolved & templates can be specialized in modules (!)
import <format>;
export template <class CharT>
struct std::formatter<Color, CharT> : std::formatter<const char*, CharT> {
    template <class FormatContext>
    auto format(const Color c, FormatContext& fc) {
        return std::formatter<const char*, CharT>::format(toString(c), fc);
    }
};

// Can be made into a stateful singleton, if on-the-fly pausing/resuming is needed.
export namespace ColoredOutput {
    // Tries enabling virtual terminal support on the first call, caching the result.
    // On failure, overrides outputting the Color values so that no garbage is printed.
    // Rarely needed explicitly.
    bool enabled();
    // Tests all supported color variations & outputs (printf, iostream & format)
    void test();
};
