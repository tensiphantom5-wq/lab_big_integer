#include "big_integer.h"
#include <algorithm>
#include <climits>
#include <stdexcept>
#include <cctype>

static void remove_zeros(std::vector<int>& a) {
    while (a.size() > 1 && a.back() == 0) {
        a.pop_back();
    }
}

static int cmp(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size()) return (a.size() < b.size() ? -1 : 1);
    for (int i = (int)a.size() - 1; i >= 0; i--) {
        if (a[i] != b[i]) return (a[i] < b[i] ? -1 : 1);
    }
    return 0;
}

static std::vector<int> add_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    int carry = 0;

    for (size_t i = 0; i < std::max(a.size(), b.size()) || carry != 0; i++) {
        int sum = carry;
        if (i < a.size()) sum += a[i];
        if (i < b.size()) sum += b[i];

        res.push_back(sum % 10);
        carry = sum / 10;
    }
    return res;
}

static std::vector<int> sub_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    int borrow = 0;

    for (size_t i = 0; i < a.size(); i++) {
        int x = a[i] - borrow;
        if (i < b.size()) x -= b[i];

        if (x < 0) {
            x += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }

        res.push_back(x);
    }

    remove_zeros(res);
    return res;
}


BigInteger::BigInteger() {
    digits_ = {0};
    negative_ = false;
}

BigInteger::BigInteger(int value) {
    if (value == INT_MIN) {
        negative_ = true;
        long long x = -(long long)value;
        while (x) {
            digits_.push_back(x % 10);
            x /= 10;
        }
        return;
    }

    negative_ = (value < 0);
    if (value < 0) value = -value;

    if (value == 0) {
        digits_ = {0};
        return;
    }

    while (value > 0) {
        digits_.push_back(value % 10);
        value /= 10;
    }
}

BigInteger::BigInteger(long long value) {
    negative_ = (value < 0);

    if (value == LLONG_MIN) {
        unsigned long long x = (unsigned long long)LLONG_MAX + 1;
        while (x) {
            digits_.push_back(x % 10);
            x /= 10;
        }
        return;
    }

    if (value < 0) value = -value;

    if (value == 0) {
        digits_ = {0};
        return;
    }

    while (value > 0) {
        digits_.push_back(value % 10);
        value /= 10;
    }
}

BigInteger::BigInteger(const std::string& s) {
    if (s.empty()) {
        digits_ = {0};
        negative_ = false;
        return;
    }

    int i = 0;
    negative_ = false;

    if (s[0] == '-') {
        negative_ = true;
        i = 1;
    } else if (s[0] == '+') {
        i = 1;
    }

    for (int j = (int)s.size() - 1; j >= i; j--) {
        if (!std::isdigit(static_cast<unsigned char>(s[j]))) {
            digits_ = {0};
            negative_ = false;
            return;
        }
        digits_.push_back(s[j] - '0');
    }

    if (digits_.empty()) digits_ = {0};

    remove_zeros(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;
}


bool BigInteger::operator==(const BigInteger& o) const {
    return negative_ == o.negative_ && digits_ == o.digits_;
}

bool BigInteger::operator!=(const BigInteger& o) const {
    return !(*this == o);
}

bool BigInteger::operator<(const BigInteger& o) const {
    if (negative_ != o.negative_) return negative_;

    int c = cmp(digits_, o.digits_);
    if (!negative_) return c < 0;
    return c > 0;
}

bool BigInteger::operator<=(const BigInteger& o) const {
    return *this < o || *this == o;
}

bool BigInteger::operator>(const BigInteger& o) const {
    return !(*this <= o);
}

bool BigInteger::operator>=(const BigInteger& o) const {
    return !(*this < o);
}


BigInteger& BigInteger::operator+=(const BigInteger& o) {
    if (negative_ == o.negative_) {
        digits_ = add_abs(digits_, o.digits_);
        return *this;
    }

    int c = cmp(digits_, o.digits_);

    if (c == 0) {
        digits_ = {0};
        negative_ = false;
    } else if (c > 0) {
        digits_ = sub_abs(digits_, o.digits_);
    } else {
        digits_ = sub_abs(o.digits_, digits_);
        negative_ = o.negative_;
    }

    return *this;
}

BigInteger BigInteger::operator+(const BigInteger& o) const {
    BigInteger r = *this;
    r += o;
    return r;
}

BigInteger& BigInteger::operator-=(const BigInteger& o) {
    *this += (-o);
    return *this;
}

BigInteger BigInteger::operator-(const BigInteger& o) const {
    BigInteger r = *this;
    r -= o;
    return r;
}


BigInteger& BigInteger::operator*=(const BigInteger& o) {
    std::vector<int> res(digits_.size() + o.digits_.size(), 0);

    for (size_t i = 0; i < digits_.size(); i++) {
        int carry = 0;
        for (size_t j = 0; j < o.digits_.size() || carry != 0; j++) {
            long long cur = res[i + j] + carry;
            if (j < o.digits_.size()) {
                cur += 1LL * digits_[i] * o.digits_[j];
            }
            res[i + j] = cur % 10;
            carry = (int)(cur / 10);
        }
    }

    remove_zeros(res);
    digits_ = res;

    negative_ = (negative_ != o.negative_);
    if (is_zero()) negative_ = false;

    return *this;
}

BigInteger BigInteger::operator*(const BigInteger& o) const {
    BigInteger r = *this;
    r *= o;
    return r;
}

BigInteger& BigInteger::operator/=(const BigInteger& o) {
    if (o.is_zero()) {
        throw std::runtime_error("Division by zero");
    }

    if (cmp(digits_, o.digits_) < 0) {
        *this = BigInteger(0);
        return *this;
    }

    bool sign = (negative_ != o.negative_);

    BigInteger a = *this;
    BigInteger b = o;
    a.negative_ = b.negative_ = false;

    std::vector<int> res;
    BigInteger cur;

    for (int i = (int)a.digits_.size() - 1; i >= 0; i--) {
        cur.digits_.insert(cur.digits_.begin(), a.digits_[i]);
        remove_zeros(cur.digits_);

        int x = 0;
        while (cmp(cur.digits_, b.digits_) >= 0) {
            cur.digits_ = sub_abs(cur.digits_, b.digits_);
            x++;
        }

        res.insert(res.begin(), x);
    }

    remove_zeros(res);
    digits_ = res;
    negative_ = sign;

    if (is_zero()) negative_ = false;

    return *this;
}

BigInteger BigInteger::operator/(const BigInteger& o) const {
    BigInteger r = *this;
    r /= o;
    return r;
}

BigInteger& BigInteger::operator%=(const BigInteger& o) {
    BigInteger q = *this / o;
    *this = *this - q * o;
    return *this;
}

BigInteger BigInteger::operator%(const BigInteger& o) const {
    BigInteger r = *this;
    r %= o;
    return r;
}


BigInteger BigInteger::operator-() const {
    BigInteger r = *this;
    if (!r.is_zero()) r.negative_ = !r.negative_;
    return r;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}


std::string BigInteger::to_string() const {
    if (is_zero()) return "0";

    std::string s;
    if (negative_) s += "-";

    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        s += char('0' + digits_[i]);
    }

    return s;
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}


std::ostream& operator<<(std::ostream& os, const BigInteger& v) {
    return os << v.to_string();
}

std::istream& operator>>(std::istream& is, BigInteger& v) {
    std::string s;
    is >> s;
    v = BigInteger(s);
    return is;
}