#include <iostream>

#include <algorithm>
#include <cstdlib>
#include <list>
#include <string>
#include <time.h>
#include <vector>
#include <typeinfo>

class StringOps
{
    using String = std::string;

    using ListString = std::list<String>;
    using VectorChar = std::vector<char>;

public:
    StringOps(const String& join, const String& split) :
        join_delimiter(join),
        split_delimiter(split),
        chars({})
        {
            const int A = ord('A');
            const int a = ord('a');
            const int d = ord('0');

            const int alpha_size = ord('z') - ord('a') + 1;
            const int digit_size = ord('9') - ord('0') + 1;
            const int diff       = a - A;

            chars.reserve(alpha_size*2 + digit_size);

            for(auto i = A; i < A + alpha_size; ++i) {
                chars.push_back(chr(i));
                chars.push_back(chr(i + diff));
            }
            for(auto i = d; i < d + digit_size; ++i) {
                chars.push_back(chr(i));
            }
        }

    String RandomString(int length, const String& prefix, const String& suffix) {
        auto s = prefix;

        for(auto i = 0; i < length; ++i) {
            s += chars[static_cast<int>(random() % chars.size())];
        }
        return s + suffix;
    }
    /*
     * @brief:    converts a list of numeric type T to a delimited string
     * @detailed:
     * @return:   delimited string
     */
    template <class T>
    String Join(const std::list<T>& values) {
        String s = "";

        for (auto v: values) {
            s += (std::to_string(v) + join_delimiter);
        }
        s.erase(s.end()-join_delimiter.size(), s.end());

        return s;
    }

    /*
     * @brief:    converts a list of strings to a delimited string
     * @detailed:
     * @return:   delimited string
     */
    String Join(const ListString& strings) {
        String s = "";

        for (auto str: strings) {
            s += (str + join_delimiter);
        }
        s.erase(s.end()-join_delimiter.size(), s.end());

        return s;
    }

    /*
     * @brief:    converts delimited string to a list of strings
     * @detailed:
     * @return:   list of strings
     */
    ListString Split(const String& s) {
        ListString strings;

        if (split_delimiter.empty())
        {
            std::cout << "EMPTY" << "\n";

            exit(-1);
        }
        size_t found = s.find(split_delimiter);
        size_t start = 0;

        while(found != String::npos) {
            strings.emplace_back(s.begin() + start, s.begin() + found);

            start = found + split_delimiter.size();
            found = s.find(split_delimiter, start);
        }
        if(start != s.size())
            strings.emplace_back(s.begin() + start, s.end());

        return strings;
    }

    template <class T>
    std::list<T> Split(const String& s)
    {
        std::list<T> values;

        size_t found = s.find(split_delimiter);
        size_t start = 0;

        while(found != String::npos) {
            auto ss = s.substr(start, found);
            auto ti = static_cast<T>(stoi(ss));

            values.emplace_back(ti);

            start = found + split_delimiter.size();
            found = s.find(split_delimiter, start);
        }
        return values;
    }

    String Reverse(const String& s)
    {
        auto r = s;

        std::reverse(r.begin(), r.end());

        return r;
    }

    inline char chr(unsigned int i)
    {
        return static_cast<char>(i);
    }

    template <class Tint>
    inline char chr(Tint i) {
        return static_cast<char>(i);
    }

    inline int ord(char c)
    {
        return static_cast<int>(c);
    }

    template <class Tint>
    inline Tint ord(char c) {
        return static_cast<Tint>(c);
    }

private:
    String join_delimiter;
    String split_delimiter;

    VectorChar chars;
};

int main()
{
    srandom(static_cast<unsigned int>(time(NULL)));

    auto str = "Answer is already there, but selected-answer uses erase function which is very costly";

    const std::type_info &si = typeid(str);

    std::cout << si.name() << "\n";

    auto so = StringOps(" | ", " ");


    auto out = so.Join(so.Split(str));

    std::cout << out << "\n";

    auto so1 = StringOps(", ", " ");

    auto numstr = "904, 867, -5309, 777";

    auto nums = so1.Split<int>(numstr);

    for(auto n: nums) {
        std::cout << n << "\n";
    }
    std::cout << so1.RandomString(8, "/tmp/", ".log") << "\n";

    auto so2 = StringOps(" | ", "");

    std::cout << so2.Join(so2.Split("abc")) << "\n";
}
