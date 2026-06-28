// -*- C++ -*-

#include "any_.h"

#include <iostream>
#include <string>

using String = std::string;

int main() {
  Map m;

  String foo("foo");
  String bar("bar");

  m.setProperty<int>(foo, 1);
  m.setProperty<String>(bar, "Hello");

  std::cout << m.getProperty<int>(foo)    << "\n";
  std::cout << m.getProperty<String>(bar) << "\n";
}