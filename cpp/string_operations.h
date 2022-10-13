#ifndef _STRING_OPERATIONS_H_
#define _STRING_OPERATIONS_H_

#include <algorithm>
#include <cstdlib>
#include <list>
#include <string>
#include <typeinfo>
#include <vector>

class StringOperations
{
    using String = std::string;

    using ListString = std::list<String>;
    using VectorChar = std::vector<char>;

public:
    StringOperations() :
        chars({}) {

        const int A = ord('A');
        const int a = ord('a');
        const int d = ord('0');

        const int alpha_size = ord('z') - ord('a') + 1;
        const int digit_size = ord('9') - ord('0') + 1;
        const int diff       = a - A;

        chars.reserve(static_cast<size_t>(alpha_size*2 + digit_size));

        for (auto i = A; i < A + alpha_size; ++i) {
            chars.emplace_back(chr(i));
            chars.emplace_back(chr(i + diff));
        }
        for (auto i = d; i < d + digit_size; ++i) {
            chars.emplace_back(chr(i));
        }
    }

    /*
     * @brief:    convert unsigned int to ASCII character
     * @detailed:
     * @return:   character
     */
    inline char chr(unsigned int i) {
        return static_cast<char>(i);
    }

    /*
     * @brief:    convert int to ASCII character
     * @detailed: template function for integer sub-type
     * @return:   character
     */
    template <typename T>
    inline char chr(T i) {
        return static_cast<char>(i);
    }

    /*
     * @brief:    convert ASCII character to int
     * @detailed:
     * @return:   integer value
     */
    inline int ord(char c) {
        return static_cast<int>(c);
    }

    /*
     * @brief:    convert ASCII character to int
     * @detailed: template function for integer sub-type
     * @return:   integer value
     */
    template <typename T>
    inline T ord(char c) {
        return static_cast<T>(c);
    }

    /*
     * @brief:    add commas to a string of decimal characters
     * @detailed:
     * @return:   delimited decimal string
     */
    String AddCommas(const String& s)
    {
        if (s.size() < 5)
            return s;

        std::string cs = "";

        for (size_t i = 0; i < s.size(); i += 3) {
            cs = ',' + s.substr(s.size() - i, 3) + cs;
        }
        auto left = s.size() % 3;

        if (left > 0)
            cs = s.substr(0, left) + cs;
        else
            cs = s.substr(0, 3) + cs;

        cs.erase(cs.end()-1, cs.end());

        return cs;
    }

    /*
     * @brief:    converts a string of hexadecimal characters into a delimited string
     * @detailed:
     * @return:   delimited hex string
     */
    String HexString(const String& s, const String& delimiter = "-") {
        String hs = "";

        for (size_t i = 0; i < s.size(); i += 4) {
            hs = (delimiter + s.substr(s.size() - i, 4)) + hs;
        }
        auto left = s.size() % 4;

        if (left > 0) {
            std::string lpad(4 - left, '0');

            hs = lpad + s.substr(0, left) + hs;
        } else
            hs = s.substr(0, 4) + hs;

        hs.erase(std::prev(hs.end(), static_cast<int>(delimiter.size())), hs.end());

        return hs;
    }

    /*
     * @brief:    join list or vector of strings with delimiter
     * @detailed:
     * @return:   delimited string
     */
    String Join(const ListString& strings, const String& delimiter) {
        String s = "";

        for (auto str: strings) {
            s += (str + delimiter);
        }
        s.erase(std::prev(s.end(), static_cast<int>(delimiter.size())), s.end());

        return s;
    }

    /*
     * @brief:    join list of integers with delimiter
     * @detailed: template function converts integer sub-type to string
     * @return:   delimited string
     */
    template <typename T>
    String Join(const std::list<T>& values, const String& delimiter) {
        String s = "";

        for (auto v: values) {
            s += (std::to_string(v) + delimiter);
        }
        s.erase(std::prev(s.end(), static_cast<int>(delimiter.size())), s.end());

        return s;
    }

    /*
     * @brief:    join vector of integers with delimiter
     * @detailed: template function converts integer sub-type to string
     * @return:   delimited string
     */
    template <class T>
    String Join(const std::vector<T>& values, const String& delimiter) {
        String s = "";

        for (auto v: values) {
            s += (std::to_string(v) + delimiter);
        }
        s.erase(std::prev(s.end(), static_cast<int>(delimiter.size())), s.end());

        return s;
    }

    /*
     * @brief:    creates a random string of input length
     * @detailed: combines prefix, random string and suffix
     * @return:   random string (e.g temp file names)
     */
    String RandomString(int length, const String& prefix, const String& suffix) {
        auto s = prefix;

        for (auto i = 0; i < length; ++i) {
            s += chars[static_cast<unsigned long>(random()) % chars.size()];
        }
        return s + suffix;
    }

    /*
     * @brief:    reverse a string
     * @detailed: non-destructive string reversal
     * @return:   reversed string
     */
    String Reverse(const String& s)
    {
        auto r = s;

        std::reverse(r.begin(), r.end());

        return r;
    }

    /*
     * @brief:    split a delimited string
     * @detailed:
     * @return:   list of strings
     */
    ListString Split(const String& s, const String& delimiter) {
        ListString ls;

        if (delimiter.empty()) {
            for (size_t i = 0; i < s.size(); ++i) {
                ls.emplace_back(s.substr(i, 1));
            }
        } else {
            size_t found = s.find(delimiter);
            size_t start = 0;

            while(found != String::npos) {
                ls.emplace_back(std::next(s.begin(), static_cast<int>(start)), std::next(s.begin() + static_cast<int>(found)));

                start = found + delimiter.size();
                found = s.find(delimiter, start);
            }
            if (start != s.size())
                ls.emplace_back(std::next(s.begin() + static_cast<int>(start)), s.end());
        }
        return ls;
    }

private:
    VectorChar chars;
};

#endif
