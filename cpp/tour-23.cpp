// -*- C++ -*-

#include <algorithm>
#include <compare>
#include <cstdlib>
#include <print>
#include <string>
#include <vector>
#include <utility>

#include "text-color.h"

using std::print;
using std::println;

using std::pair;
using std::string;
using std::vector;

using Columns = vector<int>;
using Board   = vector<Columns>;

using Row = int;
using Col = int;

using Access = int;

using RowCol   = pair<Row, Col>;
using Square   = pair<RowCol, Access>;

using Delta   = vector<RowCol>;
using Squares = vector<Square>;

constexpr int dimension_board = 8;

const string line = "+————————+————————+————————+————————+————————+————————+————————+————————+";

class Tour {
  Board board;

  const Delta delta;

public:
  Tour()
  : board({}),
    delta({RowCol(-2, -1), RowCol(-2, 1),
           RowCol(-1, -2), RowCol(-1, 2),
           RowCol( 1, -2), RowCol( 1, 2),
           RowCol( 2, -1), RowCol( 2, 1)})
  {
    board.reserve(dimension_board);

    for(auto i = 0; i < dimension_board; ++i) {
      Columns row = {};

      row.reserve(dimension_board);

      for(auto j = 0; j < dimension_board; ++j) {
        row.push_back(0);
      }
      board.push_back(row);
    }
  }

  bool solve(int x, int y, int n)
  {
    board[x][y] = n;

    if(n == dimension_board*dimension_board)
      return is_closed(x, y);

    Squares moves;

    moves.reserve(delta.size());
    moves.clear();

    for(auto d: delta) {
      auto x1 = x + d.first;
      auto y1 = y + d.second;

      if(on_board(x1, y1) && board[x1][y1] == 0) {
        moves.push_back(Square(RowCol(x1, y1), get_access(x1, y1)));
      }
    }
    sort(moves.begin(), moves.end(), [](const auto& a, const auto& b) { return a.second < b.second; });

    for(auto m: moves) {
      const auto x1 = m.first.first;
      const auto y1 = m.first.second;

      println(stderr, "TRYING... {}, {}: {}", x1, y1, n+1);

      if(solve(x1, y1, n+1))
        return true;

      println(stderr, "FAILED: {}, {}: ", x1, y1, n+1);

      board[x1][y1] = 0;
    }
    return false;
  }

  void show()
  {
    TextColor::set_bold();
    println("{}", line);

    for(auto i = 0; i < dimension_board; ++i) {
      print("|");

      for(auto j = 0; j < dimension_board; ++j) {
        print("   {:2d}   |", board[i][j]);
      }
      println("\n{}", line);
    }
  }

private:
  inline bool on_board(int x, int y) {
    return x >= 0 && x < dimension_board && y >= 0 && y < dimension_board;
  }

  bool is_closed(const int x, const int y) {
    for(auto d: delta) {
      auto x1 = x + d.first;
      auto y1 = y + d.second;

      if(on_board(x1, y1) && board[x1][y1] == 1)
        return true;
    }
    return false;
  }

  int get_access(int x, int y) {
    int count = 0;

    for(auto d: delta) {
      auto x1 = x + d.first;
      auto y1 = y + d.second;

      if(on_board(x1, y1) && board[x1][y1] == 0)
        ++count;
    }
    return count;
  }

};

int main() {
  srand(static_cast<unsigned int>(std::time(nullptr)));

  auto t = Tour();

  t.solve(rand() % dimension_board, rand() % dimension_board, 1);
  t.show();
}
