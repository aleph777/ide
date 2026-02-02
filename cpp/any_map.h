/* =============================== -*- C++ -*- ================================
  Copyright © 2026 - 2026 - <<<COMPANY>>>  All Rights Reserved.

  Redistribution of this file, in original or modified form, without
  prior written consent of 10Beauty is prohibited.

-------------------------------------------------------------------------------

============================================================================ */

#ifndef ANY_MAP_H_
#define ANY_MAP_H_

#include <any>
#include <cassert>
#include <cmath>
#include <cstring>
#include <list>
#include <ostream>
#include <string>
#include <vector>
#include <unordered_map>

namespace MyLib {

using std::list;
using std::pair;
using std::string;
using std::unordered_map;
using std::vector;

class AnyMap {

public:
  AnyMap() : m_() {}

  explicit AnyMap(const unordered_map<string, std::any> &m) : m_(m) {}

  bool operator==(const AnyMap& other) const {
    if (size() != other.size()) return false;

    for (auto& key : keys()) {
      if (!other.exists(key)) return false;
    }
    for (auto& key : other.keys()) {
      if (!exists(key)) return false;
    }
    for (auto& key : keys()) {
      if (get(key).type() != other.get(key).type())
        return false;

      const auto val1 = get(key);
      const auto val2 = other.get(key);

      if (get(key).type() == typeid(int)) {
        if (any_cast<int>(val1) == any_cast<int>(val2))
          continue;
        else
          return false;
      }
      if (get(key).type() == typeid(const char *)) {
        if (strcmp(any_cast<const char *>(val1), any_cast<const char *>(val2)) == 0)
          continue;
        else
          return false;
      }
      if (get(key).type() == typeid(string)) {
        if (any_cast<string>(val1) == any_cast<string>(val2))
          continue;
        else
          return false;
      }
      if (get(key).type() == typeid(unsigned)) {
        if (any_cast<unsigned>(val1) == any_cast<unsigned>(val2))
          continue;
        else
          return false;
      }
      if (get(key).type() == typeid(float)) {
        if (equals(any_cast<float>(val1), any_cast<float>(val2)))
          continue;
        else
          return false;
      }
      if (get(key).type() == typeid(double)) {
        if (equals(any_cast<double>(val1), any_cast<double>(val2)))
          continue;
        else
          return false;
      }
      return false;
    }
    return true;
  }

  bool operator!=(const AnyMap& other) { return !(operator==(other)); }

  std::any &operator[](string key) { return m_[key]; };

  friend std::ostream& operator<<(std::ostream& os, const AnyMap& m) {
    os << "{\n";

    for (auto p : m.key_value_pairs()) {
      os << "  {" << p.first << ", ";

      if (p.second.type() == typeid(double)) {
        os << any_cast<double>(p.second);
      } else if (p.second.type() == typeid(float)) {
        os << any_cast<float>(p.second);
      } else if (p.second.type() == typeid(int)) {
        os << any_cast<int>(p.second);
      } else if (p.second.type() == typeid(string)) {
        os << any_cast<string>(p.second);;
      } else if (p.second.type() == typeid(unsigned)) {
        os << any_cast<unsigned>(p.second);
      } else if (p.second.type() == typeid(const char *)) {
        os << any_cast<const char *>(p.second);
      }
      os << "}\n";
    }
    os << "}\n";

    return os;
  }

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
  inline bool exists(string key) const { return m_.contains(key); }

  /*
   * @brief:    gets the value for key
   * @param:    the key whose value is to be fetched
   * @return:   the value associated with key
   */
  inline std::any get(string key) const { return m_.at(key); }

  /*
   * @brief:    get the keys of the map
   * @return:   list of keys
   */
  const list<string> keys() const {
    list<string> k;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      k.emplace_back(it->first);
    }
    return k;
  }

  /*
   * @brief:    get the key/value pairs of the map
   * @return:   list of key/value pairs
   */
  const list<pair<string, std::any>> key_value_pairs() const {
    list<pair<string, std::any>> p;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      p.emplace_back(pair<string, std::any>(it->first, it->second));
    }
    return p;
  }

  /*
   * @brief:    remove the key from the map
   * @param:    key to remove
   * @return:   FOUND boolean
   */
  inline bool remove(string key) { return m_.erase(key) == 1;}

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
    inline void set(string key, std::any value) { m_[key] = value; }

  /*
   * @brief:    set the value of key
   * @param:    the key to insert
   * @param:    the associated value
   */
  inline void set(const pair<string, std::any>& lp) { set(lp.first, lp.second); }

  /*
   * @brief:    set the list of keys to value
   * @param:    the key list to insert
   * @param:    the associated value
   */
  void set(const list<string>& keys, std::any value) {
    for (auto& key : keys) {
        set(key, value);
    }
  }

  /*
   * @brief:    set the vector of keys to value
   * @param:    the key vector to insert
   * @param:    the associated value
   */
  void set(const vector<string>& keys, std::any value) {
    for (auto& key : keys) {
        set(key, value);
    }
  }

  /*
   * @brief:    set the list of keys to the associated values
   * @param:    the key list to insert
   * @param:    the value list to insert
   */
  void set(const list<string> &k, const list<std::any> &v) {
    assert(k.size() == v.size());

    auto itv = v.begin();

    for (auto itk = k.begin(); itk != k.end(); ++itk, ++itv) {
        set(*itk, *itv);
    }
  }

  /*
   * @brief:    set the vector of keys to the associated values
   * @param:    the key vector to insert
   * @param:    the value vector to insert
   */
  void set(const vector<string> &k, const vector<std::any> &v) {
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
  const list<std::any> values() {
    list<std::any> v;

    for (auto it = m_.begin(); it != m_.end(); ++it) {
      v.emplace_back(it->second);
    }
    return v;
  }

private:
  bool equals(const double a, const double b) const {
    return std::abs(a - b) < epsilon;
  }

  bool equals(const float a, const float b) const {
    return std::abs(static_cast<double>(a - b)) < epsilon;
  }

  unordered_map<string, std::any> m_;

  const double epsilon = 1e-9;

};
}
#endif
