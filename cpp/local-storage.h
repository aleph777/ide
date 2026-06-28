// -*- C++ -*-

#ifndef LOCAL_STORAGE_H_
#define LOCAL_STORAGE_H_

#include "persistent-storage.h"

#include <any>
#include <list>
#include <string>

#include "any_map.h"

namespace LocalTest {

using std::string;
using std::list;

using MyLib::AnyMap;

template <typename Data>
struct LocalStorage {
  LocalStorage() : am_() {}

  bool set(string key, const Data& d) {
    am_[key] = d;

    return true;
  }

  Data get(string key) { return am_.get(key); }

  bool exists(string key) { return am_.exists(key); }

  void clear() { am_.clear(); }

  const list<string> keys() const { return am_.keys(); }

  AnyMap am_;
};
}

#endif
