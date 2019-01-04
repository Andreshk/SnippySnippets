#pragma once
#include <cstdint> // uint32_t,int32_t,int64_t
#include <numeric> // std::gcd
#include <iostream>

class rational {
    uint32_t bits;

    int32_t n() const {
        const uint32_t sign = bits & 0x80000000U;
        const uint32_t bar = (bits >> 26) & 31U; // remove the "mantissa" and the sign bit
        const uint32_t res = ((bits & 0x3FFFFFFU) >> bar); // shift the mantissa only
        return sign ? -int32_t(res) : int32_t(res);
    }
    uint32_t d() const {
        const uint32_t bar = (bits >> 26) & 31U;
        return (1U << bar) | (bits & ((1U<<bar) - 1));
    }
    static uint32_t barPos(uint32_t d) {
        uint32_t bar = 0;
        while (d > 1 && bar < 26) { // d > 1 && bar >=26 means denominator overflow
            d >>= 1;
            ++bar;
        }
        return bar;
    }
public:
    // Fun fact: non-negative integers < 2^26 are represented identically, bitwise
    rational() : bits{ 0 } {};
    rational(const int64_t n, const int64_t d = 1) {
        const int64_t gcd = std::gcd(n, d);
        // Hope that the result of dividing by the GCD fits into 32 bits
        const uint32_t n_ = uint32_t(std::abs(n) / gcd);
        const uint32_t d_ = uint32_t(d / gcd);
        const uint32_t bar = barPos(d_);
        // perhaps add assert(n_ < (1U<<(bar+6))) 
        bits = (uint32_t(n < 0) << 31) // sign bit
             | (bar << 26)             // bar position
             | ((n_<<(bar+6))>>6)      // numerator, top bits removed in case of overflow
             | (d_&((1U<<bar) - 1));   // denominator leading bit is implicit => remove it
    }

    explicit operator float() const {
        return float(n()) / float(d());
    }

    rational& operator+=(const rational& other) {
        // Cast to the larger, signed type to avoid
        // overflow for intermediate calculations
        const int64_t n1 = this->n(), n2 = other.n();
        const int64_t d1 = this->d(), d2 = other.d();
        *this = { n1*d2 + n2*d1, d1*d2 };
        return *this;
    }

    // Favourite tricks: negation by simply flipping
    // the sign bit, modulus by clearing it.
    rational operator-() const {
        rational res{ *this };
        res.bits ^= 0x80000000U;
        return res;
    }

    friend rational abs(const rational& r) {
        rational res{ r };
        res.bits &= ~0x80000000U;
        return res;
    }

    rational& operator*=(const rational& other) {
        // Cast to the larger, signed type to avoid
        // overflow for intermediate calculations
        const int64_t n1 = this->n(), n2 = other.n();
        const int64_t d1 = this->d(), d2 = other.d();
        *this = { n1*n2, d1*d2 };
        return *this;
    }

    rational rec() const {
        const int64_t n1 = this->n(), d1 = this->d();
        // Optimisation possibility: do not calculate GCD in subsequent
        // constructor call, just place the bits where they need to be.
        if (n1 < 0)
            return { -d1,-n1 };
        else
            return { d1,n1 };
    }

    // We can now delegate subtraction and division to addition and multiplication
    rational& operator-=(const rational& other) {
        return (*this) += (-other);
    }

    rational& operator/=(const rational& other) {
        return (*this) *= other.rec();
    }

    friend std::ostream& operator<<(std::ostream&, const rational&);
    // In some .cpp:
    /*std::ostream& operator<<(std::ostream& os, const rational& rat) {
        os << rat.n() << '/' << rat.d() << ":\t";
        const uint32_t bar = (rat.bits >> 26) & 31U;
        for (int i = 31; i >= 0; --i) {
            os << ((rat.bits&(1U << i)) ? '1' : '0');
            if (i == 31 || i == 26 || (i == bar && i != 0))
                os << ' ';
        }
        return os;
    }*/
};
// Boilerplate
rational operator+(const rational& x, const rational& y) {
    return rational{ x } += y;
}
rational operator-(const rational& x, const rational& y) {
    return rational{ x } -= y;
}
rational operator*(const rational& x, const rational& y) {
    return rational{ x } *= y;
}
rational operator/(const rational& x, const rational& y) {
    return rational{ x } /= y;
}
