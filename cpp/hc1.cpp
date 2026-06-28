// -*- C++ -*-

#include <iostream>
#include <thread>

#include "thread_pool.h"

using std::cout;

int main() {
  auto hc = std::thread::hardware_concurrency();
  auto nt =   hc == 0 ? 4 : hc

  ThreadPool pool(nt);

  auto future1 = pool.enqueue([] {
    return 42;
  });

  auto future2 = pool.enqueue([](int x, int y) {
    return x + y;
  }, 10, 20);

  std::cout << future1.get() << '\n';
  std::cout << future2.get() << '\n';
}
