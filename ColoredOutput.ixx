/*
 * Simple mechanism for enabling colored output via the standard ANSI escape sequences for
 * Windows consoles that may not support it natively, by enabling virtual terminal support.
 * Also provides a small convenience enum class to represent the basic ANSI colors.
 *
 * Features: avoids printing garbage when colored output is not supported;
 * is type-safe; can be modularized (obviously); extensible - f.e. selective
 * on-the-fly colored output pausing/resuming can be easily added.
 * Using macros for the escape sequences has an occasional miniscule advantage when they're
 * fused by the preprocessor into the string literals to be outputted (but lack everything else).
 *
 * For detailed info see https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#text-formatting
 * and for some nice examples: https://superuser.com/questions/413073/windows-console-with-ansi-colors-handling/1105718#1105718
 * and https://stackoverflow.com/a/33206814.
 */
module;
// Include as little as possible from the Windows headers
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "Windows.h"

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
// Note: cannot be used outside this file until this issue is resolved:
// https://developercommunity.visualstudio.com/t/Exported-template-specialization-from-mo/1449269
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

// Implementations & helper functions below
module :private;
import <cassert>;
import <utility>; // std::pair, for ColoredOutput::test() only

const char* toString(const Color c) {
    if (!ColoredOutput::enabled()) {
        return "";
    } else switch (c) {
        case Color::Default: { return "\033[0m"; }
        case Color::Bold:    { return "\033[1m"; }
        case Color::Red:     { return "\033[31m"; }
        case Color::Green:   { return "\033[32m"; }
        case Color::Yellow:  { return "\033[33m"; }
        case Color::Blue:    { return "\033[34m"; }
        case Color::Magenta: { return "\033[35m"; }
        case Color::Cyan:    { return "\033[36m"; }
        case Color::White:   { return "\033[37m"; }
        default: { assert(false); return "\033[0m"; }
    }
}

std::ostream& operator<<(std::ostream& os, const Color c) {
    // Non-console streams do not have colored output anyways
    if (&os == &std::cout || &os == &std::cerr) {
        return (os << toString(c));
    } else {
        return os;
    }
}

// Determines whether virtual terminal support is available
// by querying ntdll.dll at run-time for the Windows version.
bool hasVirtualTerminalSupport() {
    // Access ntdll.dll only once - the result won't change during program execution :)
    static const bool res = [] {
        const DWORD minMajorVersion = 10, minMinorVersion = 0, minBuildNumber = 10586;
        const HMODULE module = GetModuleHandle(TEXT("ntdll.dll"));
        if (!module) {
            return false;
        }
        // To-do: fix includes so that NTSTATUS is defined (should be a typedef for LONG)
        typedef LONG/*NTSTATUS*/(WINAPI *RtlGetVersionPtr)(PRTL_OSVERSIONINFOW);
        const RtlGetVersionPtr RtlGetVersion = reinterpret_cast<RtlGetVersionPtr>(GetProcAddress(module, "RtlGetVersion"));
        if (!RtlGetVersion) {
            return false;
        }
        RTL_OSVERSIONINFOW osInfo;
        std::memset(&osInfo, 0, sizeof(osInfo));
        osInfo.dwOSVersionInfoSize = sizeof(osInfo);
        if (RtlGetVersion(&osInfo) != 0) {
            return false;
        }
        return (osInfo.dwMajorVersion > minMajorVersion
            || (osInfo.dwMajorVersion == minMajorVersion
                && (osInfo.dwMinorVersion > minMinorVersion
                    || (osInfo.dwMinorVersion == minMinorVersion
                        && osInfo.dwBuildNumber >= minBuildNumber))));
    }();
    return res;
}

// Whether an output handle has been redirected to a file, where color output is unsupported
bool isRedirectedToFile(const HANDLE handle) {
    return (handle != INVALID_HANDLE_VALUE
        && (GetFinalPathNameByHandle(handle, nullptr, 0, 0) != 0 /*|| GetLastError() == ERROR_INSUFFICIENT_BUFFER*/));
}

// In case the macro is unavailable on the compiling machine
#ifdef ENABLE_VIRTUAL_TERMINAL_PROCESSING
const DWORD virtualTerminalMask = ENABLE_VIRTUAL_TERMINAL_PROCESSING;
#else
const DWORD virtualTerminalMask = 0x0004;
#endif // ENABLE_VIRTUAL_TERMINAL_PROCESSING

// Attempts to enable virtual terminal support for colored output.
// Assumes colored output is supported in the first place (see hasVirtualTerminalSupport()).
bool enableVirtualTerminal(const DWORD nHandle) {
    const HANDLE handle = GetStdHandle(nHandle);
    DWORD mode = 0;
    return (!isRedirectedToFile(handle)
        && GetConsoleMode(handle, &mode)
        && SetConsoleMode(handle, mode | virtualTerminalMask));
};

// Attempts to disable virtual terminal support for the given handle.
bool disableVirtualTerminal(const DWORD nHandle) {
    const HANDLE handle = GetStdHandle(nHandle);
    assert(!isRedirectedToFile(handle));
    DWORD mode = 0;
    return (GetConsoleMode(handle, &mode)
        && SetConsoleMode(handle, mode & (~virtualTerminalMask)));
}

bool ColoredOutput::enabled() {
    // No static initialization order fiasco here - all Color outputs go
    // through toString(), which always calls this before returning anything.
    static const bool res = hasVirtualTerminalSupport()
        && enableVirtualTerminal(STD_OUTPUT_HANDLE)
        && enableVirtualTerminal(STD_ERROR_HANDLE);
    return res;
}

void ColoredOutput::test() {
    using P = std::pair<Color, const char*>;
    for (const auto [c, str] : { P{Color::Red, "red"},
                                 P{Color::Yellow, "yellow"},
                                 P{Color::Green, "green"},
                                 P{Color::Cyan, "cyan"},
                                 P{Color::Blue, "blue"},
                                 P{Color::Magenta, "magenta"},
                                 P{Color::White, "white"} })
    {
        std::cout << c << "test " << str << Color::Default << " end of test.\n";
        std::printf("%stest %s%s end of test.\n", toString(c), str, toString(Color::Default));
        std::cout << std::format("{}test {}{} end of test.\n", c, str, Color::Default);

        std::cout << c << Color::Bold << "test bright " << str << Color::Default << " end of test.\n";
        std::printf("%s%stest bright %s%s end of test.\n", toString(c), toString(Color::Bold), str, toString(Color::Default));
        std::cout << std::format("{}{}test bright {}{} end of test.\n", c, Color::Bold, str, Color::Default);
    }
}
