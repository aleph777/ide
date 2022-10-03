#include <iostream>
#include <list>
#include <string>

class StringOps
{
    using String     = std::string;
    using ListString = std::list<String>;

public:
    StringOps(String join, String split) :
        join_delimiter(join),
        split_delimiter(split)
        {}

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

private:
    String join_delimiter;
    String split_delimiter;

};

int main()
{
    auto str = "Answer is already there, but selected-answer uses erase function which is very costly";

    auto so = StringOps(" | ", " ");


    auto out = so.Join(so.Split(str));

    std::cout << out << "\n";

    auto so1 = StringOps(", ", " ");

    auto numstr = "904, 867, -5309, 777";

    auto nums = so1.Split<int>(numstr);

    for(auto n: nums) {
        std::cout << n << "\n";
    }
}
