// -*- C++ -*-

#include <iostream>
#include <string>
#include <concepts>

// 1. Define the Requirement (The Concept)
template <typename T>
concept Logger = requires(T l, std::string msg) {
    { l.log(msg) } -> std::same_as<void>;
};

// 2. Concrete Implementation (No inheritance needed!)
struct ConsoleLogger {
    void log(const std::string& msg) {
        std::cout << "[Console]: " << msg << std::endl;
    }
};

struct FileLogger {
    void log(const std::string& msg) {
        std::cout << "[File]: " << msg << std::endl;
    }
};

template <typename T>
void doit(T l, std::string msg) {
  l.log(msg);
  }

int main() {
  FileLogger fl;
  ConsoleLogger cl;

  fl.log("Hello world");
  cl.log("Hello world");

  doit(fl, "Foo");
}
