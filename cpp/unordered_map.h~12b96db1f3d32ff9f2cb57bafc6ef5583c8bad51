/*=============================================================================
  Copyright © 2022 - 2022 -  NailPro INC.  All Rights Reserved.

  Redistribution of this file, in original or modified form, without
  prior written consent of NailPro INC is prohibited.

-------------------------------------------------------------------------------

=============================================================================*/
#ifndef NAILPRO_UNORDERED_MAP_H_
#define NAILPRO_UNORDERED_MAP_H_

#include <list>
#include <ostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace MapUtils {

using std::list;
using std::pair;
using std::string;
using std::vector;

template <typename Key, typename Value>
class UnorderedMap {
  using Pair = pair<Key, Value>;

  using ListKey = list<Key>;
  using ListPair = list<Pair>;
  using ListValue = list<Value>;

  using VectorKey = vector<Key>;
  using VectorPair = vector<Pair>;
  using VectorValue = vector<Value>;

 public:
  UnorderedMap() : m_() {}

  /*
   * @brief:   clear the map
   */
  inline void clear() { m_.clear(); }

  /*
   * @brief:   check if the map is empty
   * @return:  EMPTY boolean
   */
  inline bool empty() { return m_.empty(); }

  /*
   * @brief:    checks if key exists in the map
   * @param:    the key to check
   * @return:   boolean flag
   */
  inline bool exists(Key key) const { return m_.count(key) == 1; }

  /*
   * @brief:    gets the value for key
   * @param:    the key whose value is to be fetched
   * @return:   the value associated with key
   */
  inline Value get(Key key) const { return m_.at(key); }

  /*
   * @brief:    get the keys of the map
   * @return:   list of keys
   */
  const ListKey keys() const {
    ListKey k;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      k.emplace_back(it->first);
    }
    return k;
  }

  /*
   * @brief:    get the keys of the map
   * @return:   vector of keys
   */
  inline const VectorKey keys_v() const {
    auto k = keys();

    return VectorKey(k.begin(), k.end());
  }

  /*
   * @brief:    get the key/value pairs of the map
   * @return:   list of key/value pairs
   */
  const ListPair keyval() const {
    ListPair p;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      p.emplace_back(pair<Key, Value>(it->first, it->second));
    }
    return p;
  }

  /*
   * @brief:    get the key/value pairs of the map
   * @return:   vector of key/value pairs
   */
  inline const VectorPair keyval_v() const {
    auto p = keyval();

    return VectorPair(p.begin(), p.end());
  }

  /*
   * @brief:    remove the key from the map
   * @param:    key to remove
   * @return:   FOUND boolean
   */
  inline bool remove(const Key& key) { return m_.erase(key) == 1; }

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
  inline void set(const Key& key, const Value& value) { m_[key] = value; }

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
  inline void set(const pair<Key, Value>& p) { set(p.first, p.second); }

  /*
   * @brief:    set the list of keys to value
   * @param:    the key list to insert
   * @param:    the associated value
   */
  void set(const ListKey& keys, const Value& value) {
    for (auto key : keys) {
      set(key, value);
    }
  }

  /*
   * @brief:    set the vector of keys to value
   * @param:    the key vector to insert
   * @param:    the associated value
   */
  void set(const VectorKey& keys, const Value& value) {
    for (auto key : keys) {
      set(key, value);
    }
  }

  /*
   * @brief:    set the list of keys to the associated values
   * @param:    the key list to insert
   * @param:    the value list to insert
   */
  void set(const ListKey& k, const ListValue& v) {
    for (auto itk = k.begin(), itv = v.begin(); itk != k.end(); ++itk, ++itv) {
      set(*itk, *itv);
    }
  }

  /*
   * @brief:    set the vector of keys to the associated values
   * @param:    the key vector to insert
   * @param:    the value vector to insert
   */
  void set(const VectorKey& k, const VectorValue& v) {
    for (auto itk = k.begin(), itv = v.begin(); itk != k.end(); ++itk, ++itv) {
      set(*itk, *itv);
    }
  }

  /*
   * @brief:    set the list of key/value pairs
   * @param:    the list to insert
   */
  void set(const ListPair& lp) {
    for (auto pair : lp) {
      set(pair);
    }
  }

  /*
   * @brief:    set the vector of key/value pairs
   * @param:    the vector to insert
   */
  void set(const VectorPair& vp) {
    for (auto pair : vp) {
      set(pair);
    }
  }

  /*
   * @brief:    get the number of key/value pairs in the map
   * @return:   number of key/value pairs
   */
  inline size_t size() const { return m_.size(); }

  /*
   * @brief:    get the values of the map
   * @return:   list of values
   */
  const ListValue values() {
    ListValue v;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      v.emplace_back(it->second);
    }
    return v;
  }

  /*
   * @brief:    get the values of the map
   * @return:   vector of values
   */
  inline const VectorValue values_v() {
    auto v = values();

    return VectorValue(v.begin(), v.end());
  }

  bool operator==(const UnorderedMap<Key, Value>& other) const {
    if (size() != other.size()) return false;

    for (auto key : keys()) {
      if (!other.exists(key)) return false;
    }
    for (auto key : other.keys()) {
      if (!exists(key)) return false;
    }
    for (auto key : keys()) {
      if (get(key) != other.get(key)) return false;
    }
    return true;
  }

  bool operator!=(const UnorderedMap<Key, Value>& other) {
    return !(operator==(other));
  }

  Value& operator[](Key key) { return m_[key]; };

  friend std::ostream& operator<<(std::ostream& os,
                                  const UnorderedMap<Key, Value>& m) {
    os << "{\n";

    for (auto p : m.keyval()) {
      os << "  {" << p.first << ", " << p.second << " }\n";
    }
    os << "}\n";

    return os;
  }

 private:
  std::unordered_map<Key, Value> m_;
};

}  // namespace MapUtils
#endif
