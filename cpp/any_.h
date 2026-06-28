// -*- C++ -*-

#ifndef ANY_H_
#define ANY_H_

#include <string>
#include <unordered_map>
#include <any>

class Map {
    std::unordered_map<std::string, std::any> properties;

public:
    template<typename T>
    void setProperty(const std::string& key, T value) {
        properties[key] = value;
    }

    template<typename T>
    T getProperty(const std::string& key) {
        return std::any_cast<T>(properties[key]);
    }
};

#endif
