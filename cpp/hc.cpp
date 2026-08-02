// -*- C++ -*-

#include <list>
#include <print>
#include <thread>

#include "thread_pool.h"

using std::list;
using std::println;

int foo(int a, int b)
{
  return a * b;
}

int bar(int a, int b)
{
  return 0;
}

int main()
{
  auto hc = std::thread::hardware_concurrency();
  auto nt = hc == 0 ? 4 : hc;

  ThreadPool pool(nt);

  // Enqueue a simple lambda
  auto result1 = pool.enqueue([](int a, int b) { return a + b; }, 10, 20);
  auto result2 = pool.enqueue([](int a, int b) { return a + b; }, 35, 20);

  println("Sum Result 1: {}", result1.get());
  println("Sum Result 2: {}", result2.get());
  println("-----");
  {
    list<decltype(result1)> results;

    for (auto i = 1; i <= 10; ++i)
    {
      results.emplace_back(pool.enqueue([](int a, int b) { return a + b; }, i * 10, i * 20));
    }
    for (auto& result : results)
    {
      println("Sum Result: {}", result.get());
    }
  }
  {
    // std::function<void(int)> f_display_obj = PrintNum();
    // f_display_obj(18);

    std::function<int(int, int)> f = std::bind(foo, std::placeholders::_1, std::placeholders::_2);
    std::function<int(int, int)> g = std::bind(foo, std::placeholders::_1, std::placeholders::_2);

    auto r = pool.enqueue(g, 0, 0);

    list<decltype(r)> results;

    println("-----");

    for (auto i = 1; i <= 10; ++i)
    {
      results.emplace_back(pool.enqueue(f, i, i));
    }
    for (auto& result : results)
    {
      println("Sum Result: {}", result.get());
    }
  }
}
