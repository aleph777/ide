#include <algorithm>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>

#define DIMENSION_BOARD 8

#define LINE "+--------+--------+--------+--------+--------+--------+--------+--------+\n"

using namespace std;

using Columns = vector<string>;
using Board   = vector<Columns>;

using Row = int;
using Col = int;

using RowCol = pair<Row, Col>;
using Delta  = vector<RowCol>;

class Queens
{
    Board board;
    Delta delta;

public:
    Queens() :
        board({}),
        delta({})
    {
        board.reserve(DIMENSION_BOARD);
        delta.reserve(8*DIMENSION_BOARD-1);

        for(auto i = 0; i < DIMENSION_BOARD; ++i)
        {
            Columns row = {};

            row.reserve(DIMENSION_BOARD);

            for(auto j = 0; j < DIMENSION_BOARD; ++j)
            {
                row.push_back("");
            }
            board.push_back(row);

            for(auto k = 1; k < DIMENSION_BOARD; ++k)
            {
                delta.push_back(RowCol( 0,  k));
                delta.push_back(RowCol( 0, -k));
                delta.push_back(RowCol( k,  0));
                delta.push_back(RowCol(-k,  0));
                delta.push_back(RowCol( k,  k));
                delta.push_back(RowCol( k, -k));
                delta.push_back(RowCol(-k,  k));
                delta.push_back(RowCol(-k, -k));
            }
        }
    }

    bool solve(int x, int y, int n)
    {
        char q[8];

        sprintf(q, "   Q%d ", n);

        board[x][y] = q;

        for(auto d: delta)
        {
            auto x1 = x + d.first;
            auto y1 = y + d.second;

            if(on_board(x1, y1) && board[x1][y1] != "")
            {
                board[x][y] = "";

                return false;
            }
        }
        if(n == DIMENSION_BOARD)
            return true;

        for(auto i = 0; i < DIMENSION_BOARD; ++i)
        {
            cout << i << ", " << y+1 << "\n";

            if(board[i][y+1] == "" && solve(i, y+1, n+1))
                return true;
        }
        board[x][y] = "";

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
                char buffer[8];

                sprintf(buffer, "%6s", board[i][j].c_str());

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

};

int main()
{
    auto queens = Queens();

    queens.solve(0, 0, 1);
    queens.show();
}
