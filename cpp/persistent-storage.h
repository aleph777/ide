// -*- C++ -*-

#ifndef PERSISTENT_STORAGE_H_
#define PERSISTENT_STORAGE_H_

#include <concepts>
#include <string>

namespace LocalTest {

using std::string;

template <typename T, typename Data>
concept PersistentStorage = requires(T s, const Data &d, string key) {
  { s.set(key, d) } -> std::same_as<bool>;
  { s.get(key) }    -> std::same_as<Data>;
  { s.exists(key) } -> std::same_as<bool>;
  { s.clear() }     -> std::same_as<void>;
};
}

#endif
