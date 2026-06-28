// -*- C++ -*-

#include <format>
#include <generator> // C++23 feature
#include <iostream>
#include <limits>
#include <ostream>
#include <ranges> // For views
#include <string>
#include <algorithm>

using std::cout;
using std::format;
using std::string;
                     //
// A clean, sequential number generator
std::generator<int> count_up(unsigned start, unsigned end) {
    for (unsigned i = start; i <= end ; ++i) {
        co_yield i;
    }
}
int max_val = std::numeric_limits<unsigned>::max();

const string get_id() {
  for (auto x : count_up(1, max_val)) {
    return format("ID{:0>8}\n", x);
  }
  return string();
}

int main() {
  // std::generator is compatible with range-based for loops!

  for (int i = 1; i <= 10; ++i) {
    cout << get_id();
  }
  return 0;
}
