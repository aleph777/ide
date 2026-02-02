// -*- C++ -*-

#ifndef UNORDERED_MAP_H_
#define UNORDERED_MAP_H_

#include <unordered_map>

#include <algorithm>
#include <cassert>
#include <list>
#include <ostream>
#include <ranges>
#include <utility>
#include <vector>

namespace map_utils {

template <typename T1, typename T2>
class UnorderedMap {
public:
  UnorderedMap() : m_() {}

  explicit UnorderedMap(const std::unordered_map<T1, T2>& m) : m_(m) {}

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
  inline bool exists(T1 key) const { return m_.count(key) == 1; }

  /*
   * @brief:    gets the value for key
   * @param:    the key whose value is to be fetched
   * @return:   the value associated with key
   */
  inline T2 get(T1 key) const { return m_.at(key); }

  /*
   * @brief:    get the keys of the map
   * @return:   list of keys
   */
  const std::list<T1> keys() const {
    std::list<T1> list_keys;

    for (const auto& key : m_ | std::views::keys) {
      list_keys.emplace_back(key);
    }
    return list_keys;
  }

  /*
   * @brief:    get the key/value pairs of the map
   * @return:   list of key/value pairs
   */
  const std::list<std::pair<T1, T2>> key_value_pairs() const {
    std::list<std::pair<T1, T2>> list_pairs;

    for (auto const &[key, value] : m_) {
      list_pairs.emplace_back(std::pair<T1, T2>(key, value));
    }
    return list_pairs;
  }

  /*
   * @brief:    remove the key from the map
   * @param:    key to remove
   * @return:   FOUND boolean
   */
  inline bool remove(T1 key) { return m_.erase(key) == 1;}

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
    inline void set(T1 key, T2 value) { m_[key] = value; }

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
    inline void set(const std::pair<T1, T2>& lp) { set(lp->first, lp->second); }

  /*
   * @brief:    set the list of keys to value
   * @param:    the key list to insert
   * @param:    the associated value
   */
  void set(const std::list<T1>& keys, T2 value) {
    for (auto key : keys) {
        set(key, value);
    }
  }

  /*
   * @brief:    set the vector of keys to value
   * @param:    the vector of keys to insert
   * @param:    the associated value
   */
  void set(const std::vector<T1>& keys, T2 value) {
    for (auto key : keys) {
        set(key, value);
    }
  }

  /*
   * @brief:    set the list of keys to the associated values
   * @param:    the key list to insert
   * @param:    the value list to insert
   */
  void set(const std::list<T1> &k, const std::list<T2> &v) {
    assert(k.size() == v.size());

    for (auto itk = k.begin(), itv = v.begin(); itk != k.end(); ++itk, ++itv) {
        set(*itk, *itv);
    }
  }

  /*
   * @brief:    set the vector of keys to value
   * @param:    the vector of keys to insert
   * @param:    the vector of values to insert
   */
  void set(const std::vector<T1>& k, const std::vector<T2>& v) {
    assert(k.size() == v.size());

    for (auto i = 0; i < static_cast<int>(k.size()); ++i) {
        set(k[i], v[i]);
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
  const std::list<T2> values() {
    std::list<T2> list_values;

    for (const auto& value : m_ | std::views::values) {
      list_values.emplace_back(value);
    }
    return list_values;
  }

  bool operator==(const UnorderedMap<T1, T2>& other) const {
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

  bool operator!=(const UnorderedMap<T1, T2>& other) { return !(operator==(other)); }

  T2& operator[](T1 key) { return m_[key]; };

  friend std::ostream& operator<<(std::ostream& os, const UnorderedMap<T1, T2>& m) {
      os << "{\n";
    for (auto p : m.keyval()) {
      os << "  {" << p.first << ", " << p.second << " }\n";
    }
    os << "}\n";

    return os;
  }

 private:
  std::unordered_map<T1, T2> m_;
};

}  // namespace map_utils
#endif
