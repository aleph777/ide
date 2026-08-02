// -*- C++ -*-

#ifndef TEXT_COLOR_H_
#define TEXT_COLOR_H_

#include <print>
#include <string>

namespace TextColor {

constexpr std::string normal = "\e[0m";
constexpr std::string bold   = "\e[1m";

constexpr std::string fg_black   = "\e[90m";
constexpr std::string fg_red     = "\e[91m";
constexpr std::string fg_green   = "\e[92m";
constexpr std::string fg_yellow  = "\e[93m";
constexpr std::string fg_blue    = "\e[94m";
constexpr std::string fg_magenta = "\e[95m";
constexpr std::string fg_cyan    = "\e[96m";
constexpr std::string fg_white   = "\e[97m";

constexpr std::string bg_black   = "\e[40m";
constexpr std::string bg_red     = "\e[101m";
constexpr std::string bg_green   = "\e[102m";
constexpr std::string bg_yellow  = "\e[103m";
constexpr std::string bg_blue    = "\e[104m";
constexpr std::string bg_magenta = "\e[105m";
constexpr std::string bg_cyan    = "\e[106m";
constexpr std::string bg_white   = "\e[107m";

inline void set(const std::string& attribute) {
  std::print("{}",attribute);
}

inline void set(const std::string& attribute, const std::string& color) {
  std::print("{}",attribute+color);
}

inline void set(const std::string &attribute, const std::string& color1, const std::string& color2) {
  std::print("{}",attribute+color1+color2);
}

inline void reset() {
  set(normal);
}

inline void set_bold() {
  set(bold);
}

}
#endif
