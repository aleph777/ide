#include <iostream>
#include <assert.h>

#include "map.h"

#define COUT(x) std::cout << x << "\n";

using MapStringInt = map_utils::Map<std::string, int>;

int main() {
    MapStringInt m;

    m.set("paste", -2);
    m.set("fudge", 17);

    assert(m.exists("fudge"));
    assert(m.exists("paste"));
    assert(!m.exists("spoon"));
    assert(m.size() == 2);
    assert(m.get("fudge") == 17);
    assert(m.get("paste") == -2);

    std::list<std::string> l = {"AB", "CD", "EF"};
    std::list<int>         i = {  1,    2,    3};

    m.set(l, i);

    assert(m.get("AB") == 1);
    assert(m.get("CD") == 2);
    assert(m.get("EF") == 3);

    auto k = m.keys();
    auto v = m.values();

    assert(m.size() == 5);
    assert(k.size() == 5);
    assert(v.size() == 5);

    m.set(l, -17);

    assert(m.get("AB") == -17);
    assert(m.get("CD") == -17);
    assert(m.get("EF") == -17);

    m.set("EF", 86);

    assert(m.get("EF") == 86);

    assert(m.size() == 5);
    assert(k.size() == 5);
    assert(v.size() == 5);


    auto p = m.keyval();

    assert(p.size() == 5);

    std::vector<std::string> v1 = {"BA", "DC", "EF"};
    std::vector<int> i1 = { -1, -2, -3};

    m.set(v1, i1);

    auto k2 = m.keys();
    auto v2 = m.values();

    assert(m.size() == 7);
    assert(k2.size() == 7);
    assert(v2.size() == 7);

    assert(m.get("BA") == -1);
    assert(m.get("DC") == -2);
    assert(m.get("EF") == -3);

    assert(m.remove("EF"));
    assert(!m.remove("GH"));

    assert(m.size() == 6);
    assert(!m.exists("EF"));
    assert(!m.empty());

    m.clear();

    assert(m.size() == 0);
    assert(m.empty());

    MapStringInt m1;
    MapStringInt m2;

    m1.set("a", -1);
    m2.set("a", -1);

    assert(m1 == m2);

    m1.set("BB", 22);

    assert(m1 != m2);

    m2.set("BB", 22);

    assert(m1 == m2);

    m1.set("x47", 12);
    m2.set("b52", 39);

    assert(m1 != m2);

    m1.set("b52", 39);
    m2.set("x47", 12);

    assert(m1 == m2);

    std::cout << m1 << "--------------------------------\n";

    auto m4 = m2;

    std::cout << m4 << "--------------------------------\n";

    map_utils::Map<std::string, MapStringInt> m3;

    m2["x47"] = -47;

    std::cout << m2 << "--------------------------------\n";

    m3.set("BB", MapStringInt(m2));

    m2["x47"] = 99;

    assert(m3.exists("BB"));

    std::cout << m3;

    assert(m3["BB"]["x47"] == -47);
}
