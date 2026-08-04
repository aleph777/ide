#include "sudoku.hpp"

#include <algorithm>
#include <random>

namespace sudoku
{

Sudoku::Sudoku(unsigned revealed)
    : revealed_(revealed),
      sqmap_({{1, 1}, 1},
             {{1, 2}, 1},
             {{1, 3}, 1},
             {{2, 1}, 1},
             {{2, 2}, 1},
             {{2, 3}, 1},
             {{3, 1}, 1},
             {{3, 2}, 1},
             {{3, 3}, 1},
             {{1, 4}, 2},
             {{1, 5}, 2},
             {{1, 6}, 2},
             {{2, 4}, 2},
             {{2, 5}, 2},
             {{2, 6}, 2},
             {{3, 4}, 2},
             {{3, 5}, 2},
             {{3, 6}, 2},
             {{1, 7}, 3},
             {{1, 8}, 3},
             {{1, 9}, 3},
             {{2, 7}, 3},
             {{2, 8}, 3},
             {{2, 9}, 3},
             {{3, 7}, 3},
             {{3, 8}, 3},
             {{3, 9}, 3}),
      row_(),
      col_(),
      square_(),
      board_(),
      available_()
{
  init();
}

void Sudoku::init()
{
  for (auto r = 1; r <= 9; ++r)
  {
    for (auto c = 1; c <= 9; ++c)
    {
      const RowCol rc({r, c});

      row_.set(rc, true);
      col_.set(rc, true);
      square_.set(rc, true);
      board_.set(rc, 0);

      available_[rc].fill(0);
    }
  }
  SeedBoard(1, 3);
  SeedBoard(4, 6);
  SeedBoard(7, 9);
}

void Sudoku::SeedBoard(unsigned from, unsigned to)
{
  std::random_device rd;
  std::mt19937 gen(rd());

  for (auto r = from; r <= to; ++r)
  {
    for (auto c = from; c <= to; ++c)
    {
      std::uniform_int_distribution<> distr(0, static_cast<int>(an.size()) - 1);

      const RowCol rc({r, c});

      const int num = distr(gen);

      board_.set(rc, num);

      const RowNum rn({r, num});
      const ColNum cn({c, num});

      row_.set(rn, false);
      col_.set(cn, false);

      const MapRowColSquare square = sqmap_.get(rc);
      const SqrNum sn({square, num});

      square_.set(sn, false);

      available_.remove(rc);
    }
  }
}

void Sudoku::ProcessAvailable()
{
  for (const auto& key : available_.keys())
  {
    const Row row = key->first;
    const Col col = key->second;

    available_[key].clear();

    for (auto n = 1; n <= 9; ++n)
    {
      const RowNum rn({row, n});

      if (!row_.get(rn))
        continue;

      const ColNum cn({col, n});

      if (!col_.get(cn))
        continue;

      const Sqaure sqaure(sqmap_.get(rc));
      const SqrNum sn({square, num});

      if (!square_.get(sn))
        continue;

      available_[key].emplace_back(n);
    }
  }
}

bool Sudoku::PlaceNumber()
{
  ProcessAvailable();

  auto keys(available_.keys());

  std::sort(keys.begin(), keys.end(),
            [](const RowCol& a, const RowCol& b) { return available_.get(a) < available_.get(b); });

  const auto key = keys.first();

  if (available_[key].size() == 0)
    return false;

  const auto num = available_[key]
      // std::vector<int> numbers = {5, 2, 8, 1, 9};
      // std::sort(numbers.begin(), numbers.end(), [](int a, int b) {
      //     return a > b;
      // });

      ArrayRowCol arc(keys.begin(), keys.end(),
                      [](const RowCol& a, const RowCol& b) { return available_.get(a) < available_.get(b); });

  // std::vector<T> v{ std::begin(l), std::end(l) };

  // std::sort(s.begin(), s.end(), std::greater<int>());
  std::sort()

      VectorNumber tmp;

  tmp.reserve(9);
}
}  // namespace sudoku
