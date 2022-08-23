#include <algorithm>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>

#define DIMENSION_BOARD 8

#define LINE "+--------+--------+--------+--------+--------+--------+--------+--------+\n"

using namespace std;

using Columns = vector<int>;
using Board   = vector<Columns>;

using Row = int;
using Col = int;

using Access = int;

using RowCol   = pair<Row, Col>;
using Square   = pair<RowCol, Access>;

using Delta   = vector<RowCol>;
using Squares = vector<Square>;

class Tour
{
    const Delta delta;

    Board board;

public:
    Tour() :
        delta({RowCol(-2, -1), RowCol(-2, 1),
               RowCol(-1, -2), RowCol(-1, 2),
               RowCol(1, -2),  RowCol(1,  2),
               RowCol(2, -1),  RowCol(2,  1)}),
        board({})
        {
            board.reserve(DIMENSION_BOARD);

            for(auto i = 0; i < DIMENSION_BOARD; ++i)
            {
                Columns row = {};

                row.reserve(DIMENSION_BOARD);

                for(auto j = 0; j < DIMENSION_BOARD; ++j)
                {
                    row.push_back(0);
                }
                board.push_back(row);
            }
        }

    bool solve(int x, int y, int n)
    {
        board[x][y] = n;

        if(n == DIMENSION_BOARD*DIMENSION_BOARD)
            return is_closed(x, y);

        Squares moves;

        moves.reserve(delta.size());
        moves.clear();

        for(auto d: delta)
        {
            auto x1 = x + d.first;
            auto y1 = y + d.second;

            if(on_board(x1, y1) && board[x1][y1] == 0)
            {
                moves.push_back(Square(RowCol(x1, y1), get_access(x1, y1)));
            }
        }
        sort(moves.begin(), moves.end(), [](const auto& a, const auto& b) { return a.second < b.second; });

        for(auto m: moves)
        {
            const auto x1 = m.first.first;
            const auto y1 = m.first.second;

            cout << "TRYING... " << x1 << ", " << y1 << ": " << n+1 << "\n";

            if(solve(x1, y1, n+1))
                return true;

            cout << "FAILED: " << x1 << ", " << y1 << ": " << n+1 << "\n";

            board[x1][y1] = 0;
        }
        return false;

    }

    void show()
    {
        cout << "\n" << LINE;

        for(auto i = 0; i < DIMENSION_BOARD; ++i)
        {
            cout << "|";

            for(auto j = 0; j < DIMENSION_BOARD; ++j)
            {
                char buffer[16];

                sprintf(buffer, "  %2d  ", board[i][j]);

                cout << buffer << "  |";
            }
            cout << "\n" << LINE;
        }
    }

private:
    inline bool on_board(int x, int y)
    {
        return x >= 0 && x < DIMENSION_BOARD && y >= 0 && y < DIMENSION_BOARD;
    }

    bool is_closed(const int x, const int y)
    {
        for(auto d: delta)
        {
            auto x1 = x + d.first;
            auto y1 = y + d.second;

            if(on_board(x1, y1) && board[x1][y1] == 1)
                return true;
        }
        return false;
    }

    int get_access(int x, int y)
    {
        int count = 0;

        for(auto d: delta)
        {
            auto x1 = x + d.first;
            auto y1 = y + d.second;

            if(on_board(x1, y1) && board[x1][y1] == 0)
                ++count;
        }
        return count;
    }

};

int main()
{
    srand(static_cast<unsigned int>(std::time(nullptr)));

    auto t = Tour();

    t.solve(rand() % DIMENSION_BOARD, rand() % DIMENSION_BOARD, 1);
    t.show();
}
