#include "any_map.h"

#include <iostream>
#include <string>

using namespace MyLib;

using std::cout;
using std::list;
using std::pair;
using std::string;

using anyList   = list<std::any>;
// using anyVector = vector<std::any>;

int main() {
  auto am = AnyMap();
  auto st1 = string("key1");
  auto st2 = string("key2");

  auto alk = list<string>({"A", "B", "F"});
  auto alv = anyList({1, 2, 1.75});


  pair<string, int> p;

  p.first  = "Hello";
  p.second = 2;

  // am[st1] = 1;
  // am[st2] = string("world");

  am.set(alk, 2.5);

  cout << am;

  am.set(st1, 1);
  am.set(st2, string("world"));
  am.set(p);
  am.set(alk, alv);

  cout << am;

  const char *str = "zxcvbnm";

  am["charstar"] = str;

  if (am["charstar"].type() == typeid(const char *))
    cout << any_cast<const char *>(am["charstar"]) << "\n";
  else
    cout << "poop\n";

  for (auto &key : am.keys()) {
    cout << "KEY: " << key << "\n";
  }
}
