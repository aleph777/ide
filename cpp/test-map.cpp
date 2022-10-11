#include <iostream>

// #include "./unordered_map.h"
#include "./map.h"

#define COUT(x) std::cout << x << "\n";

int main() {
    // UnorderedMap<std::string, int> m;
    Map<std::string, int> m;

    m.insert("paste", -2);
    m.insert("fudge", 17);

    if (m.exists("fudge"))
        COUT("fudge: " << m.get("fudge"));

    if (m.exists("spoon"))
        COUT("fudge: " << m.get("spoon"));

    if (m.exists("spoon"))
        COUT("how did I get spoon?");

    COUT("size: " << m.size());

    for (auto k: m.keys())
        COUT("key: " << k);

    for (auto v: m.values())
        COUT("value: " << v);
}
