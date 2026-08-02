// -*- C++ -*-

#include <array>
#include <print>
#include <string>
#include <utility>
#include <vector>

#include "text-color.h"

using std::print;
using std::println;

using std::array;
using std::pair;
using std::string;
using std::vector;

constexpr int dimension_board = 8;

const string line = "+————————+————————+————————+————————+————————+————————+————————+————————+";

using Columns = array<string, dimension_board>;
using Board = array<Columns, dimension_board>;

using Row = int;
using Col = int;

using RowCol = pair<Row, Col>;
using Delta = vector<RowCol>;

class Queens
{
    Board board;
    Delta delta;

  public:
    Queens() : board({}), delta({})
    {
        delta.reserve(dimension_board * dimension_board - 1);

        for (auto i = 0; i < dimension_board; ++i)
        {
            for (auto j = 0; j < dimension_board; ++j)
            {
                board[i][j] = "";
            }
            for (auto k = 1; k < dimension_board; ++k)
            {
                delta.push_back(RowCol(0, k));
                delta.push_back(RowCol(0, -k));
                delta.push_back(RowCol(k, 0));
                delta.push_back(RowCol(-k, 0));
                delta.push_back(RowCol(k, k));
                delta.push_back(RowCol(k, -k));
                delta.push_back(RowCol(-k, k));
                delta.push_back(RowCol(-k, -k));
            }
        }
    }

    bool solve(int x, int y, int n)
    {
        char q[8];

        sprintf(q, "   Q%d ", n);

        board[x][y] = q;

        for (auto d : delta)
        {
            auto x1 = x + d.first;
            auto y1 = y + d.second;

            if (on_board(x1, y1) && board[x1][y1] != "")
            {
                board[x][y] = "";

                return false;
            }
        }
        if (n == dimension_board)
            return true;

        for (auto i = 0; i < dimension_board; ++i)
        {
            println(stderr, "{}, {}", i, y + 1);

            if (board[i][y + 1] == "" && solve(i, y + 1, n + 1))
                return true;
        }
        board[x][y] = "";

        return false;
    }

    void show()
    {
        TextColor::set_bold();
        println("{}", line);

        for (auto i = 0; i < dimension_board; ++i)
        {
            print("|");

            for (auto j = 0; j < dimension_board; ++j)
            {
                print("{:6s}  |", board[i][j].c_str());
            }
            println("\n{}", line);
        }
        TextColor::reset();
    }

  private:
    inline bool on_board(int x, int y)
    {
        return x >= 0 && x < dimension_board && y >= 0 && y < dimension_board;
    }
};

int main()
{
    auto queens = Queens();

    queens.solve(0, 0, 1);
    queens.show();
}
