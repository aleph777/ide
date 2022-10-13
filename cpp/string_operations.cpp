#include <iostream>
#include <cstdlib>
#include <cstring>
#include <time.h>

#include <list>
#include <vector>

#include "string_operations.h"

template <typename T>
inline T ston(const std::string& s) {
    const std::type_info &ti = typeid(T);

    constexpr char si = 'i';
    constexpr char sj = 'j';
    constexpr char sl = 'l';
    constexpr char sm = 'm';
    constexpr char sx = 'x';
    constexpr char sy = 'y';
    constexpr char sf = 'f';
    constexpr char sd = 'd';
    constexpr char se = 'e';

    auto *x = ti.name();

    if (strcmp(ti.name(), &si) || (strcmp(ti.name(), &sj))) {
        return static_cast<T>(stoi(s));
    }
    if (strcmp(ti.name(), &sl)) {
        return static_cast<T>(stol(s));
    }
    if (strcmp(ti.name(), &sm)) {
        return static_cast<T>(stoul(s));
    }
    if (strcmp(ti.name(), &sx)) {
        return static_cast<T>(stoll(s));
    }
    if (strcmp(ti.name(), &sy)) {
        return static_cast<T>(stoull(s));
    }
    if (strcmp(ti.name(), &sf)) {
        return static_cast<T>(stof(s));
    }
    if (strcmp(ti.name(), &sd)) {
        return static_cast<T>(stod(s));
    }
    if (static_cast<char>(x),
        strcmp(ti.name(), &se)) {
        return static_cast<T>(stold(s));
    }
    std::cerr << "ston: ERROR, type not supported" << "\n";

    return 0;
}


template <typename T>
std::list<T> foo(const std::string& s, const std::string& delimiter)
{
    std::list<T> l;

    if (delimiter.empty()) {
        for (size_t i = 0; i < s.size(); ++i) {
            l.emplace_back(static_cast<T>(stoi(s.substr(i, 1))));
        }
    } else {
        size_t found = s.find(delimiter);
        size_t start = 0;

        while(found != std::string::npos) {
            auto ss = s.substr(start, found);
            auto ti = static_cast<T>(stoi(ss));

            l.emplace_back(ti);

            start = found + delimiter.size();
            found = s.find(delimiter, start);
        }
        if (start != s.size()) {
            l.emplace_back(static_cast<T>(stoi(s.substr(start, s.size()))));
        }
    }
    return l;
}

#define COUT(x) std::cout << x << "\n"

int main() {
    srandom(static_cast<unsigned int>(time(NULL)));


    auto so = StringOperations();

    // COUT(so.Join(so.Split("abcdef", ""), " | "));

    std::list<int> x = foo<int>("123 345 678", " ");

    COUT(so.Join<int>(x, " | "));

    // std::list<int> l = { 1, 2, 3 };
    // std::vector<int> v = { 1, 2, 3 };
    // std::vector<std::string> s = { "abd", "def" };

    // const std::type_info &li = typeid(l.front());
    // const std::type_info &vi = typeid(v.front());
    // const std::type_info &si = typeid(s.front());

    // COUT(li.name());
    // COUT(vi.name());
    // COUT(si.name());
    // COUT(typeid(so).name());

    // COUT(so.HexString("123DEADBEEF"));
    // COUT(so.AddCommas("9412345"));
}
