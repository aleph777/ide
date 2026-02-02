// -*- C++ -*-

#ifndef PET_STORAGE_H_
#define PET_STORAGE_H_

#include "persistent-storage.h"

#include <list>
#include <string>

#include "unordered_map.h"

namespace LocalTest {

using std::string;
using std::list;

using map_utils::UnorderedMap;

template <typename Data>
struct PetStorage {
  PetStorage() : ps_() {}

  bool set(string key, const Data& d) {
    ps_[key] = d;

    return true;
  }

  Data get(string key) { return ps_.get(key); }

  bool exists(string key) { return ps_.exists(key); }

  void clear() { ps_.clear(); }

  const list<string> keys() const { return ps_.keys(); }

  UnorderedMap<string, Data> ps_;
};

}

#endif
