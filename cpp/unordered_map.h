#ifndef _UTILS_UNORDERED_MAP_H_
#define _UTILS_UNORDERED_MAP_H_

#include <unordered_map>
#include <algorithm>
#include <list>
#include <numeric>

template <typename T1, typename T2>
class UnorderedMap
{
public:
    UnorderedMap() : m_({}) {}

    inline bool exists(T1 key) {
        return m_.find(key) != m_.end();
    }

    inline T2 get(T1 key) {
        return m_[key];
    }

    inline void insert(T1 key, T2 value) {
        m_[key] = value;
    }

    const std::list<T1> keys() {
        std::list<T1> k;

        for (auto it = m_.begin(); it != m_.end(); ++it) {
            k.emplace_back(it->first);
        }
        return k;
    }

    inline int size() {
        int count = 0;

        for (auto it = m_.begin(); it != m_.end(); ++it)
            ++count;

       return count;

    }

    const std::list<T2> values() {
        std::list<T2> v;

        for (auto it = m_.begin(); it != m_.end(); ++it) {
            v.emplace_back(it->second);
        }
        return v;
    }

private:
    std::unordered_map<T1, T2> m_;

};

#endif
