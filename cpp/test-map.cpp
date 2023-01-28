#include <iostream>

// #include "./unordered_map.h"
#include "./map.h"

#define COUT(x) std::cout << x << "\n";

int main() {
    // UnorderedMap<std::string, int> m;
    Map<std::string, int> m;

    m.insert("paste", -2);
    m.insert("fudge", 17);

    std::string key = "fudge";

    if (m.exists(key))
        COUT(key + ": " << m.get(key));
    else
        COUT("FAIL: " + key + " not found");

    key = "paste";

    if (m.exists(key))
        COUT(key + ": " << m.get(key));
    else
        COUT("FAIL: " + key + " not found");

    key = "spoon";

    if (m.exists(key))
        COUT(key + ": " << m.get(key));
    else
        COUT(key + " not found");

    COUT("size: " << m.size());

    for (auto k: m.keys())
        COUT("key: " << k);

    for (auto v: m.values())
        COUT("value: " << v);
}
