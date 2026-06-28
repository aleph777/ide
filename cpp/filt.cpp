// -*- C++ -*-

#include <algorithm>
#include <format>
#include <functional>
#include <list>
#include <print>
#include <ranges>
#include <vector>

std::vector<int> numbers = {1, 2, 3, 4, 5, 6};

auto evens = [](int n) { return (n % 2) == 0; };
auto odds = [](int n) { return (n % 2) == 1; };

auto foo = [](int n) { return n * 2; };

template <std::ranges::range Range> void p(Range v) {
  std::print("{} ", "{");
  for (auto element : v) {
    std::print("{} ", element);
  }
  std::println("{}", "}");
}

using std::views::filter;

int main() {
  auto e = numbers | filter(evens);
  auto o = numbers | filter(odds);

  p(e);
  p(o);

  std::ranges::transform(e, e.begin(), foo);

  p(numbers);
  p(e);

  std::vector<int> v(o.begin(), o.end());
  std::list<int> l(e.begin(), e.end());

  p(v);
  p(l);

  std::println("{:3}", 2);
}
