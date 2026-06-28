// -*- C++ -*-

#ifndef SUDOKU_H_
#define SUDOKU_H_

#include "unordered_map_h_"

#include <array>
#include <pair>
#include <vector>

namespace sudoku {

using map_utils;
using std::array;
using std::pair;
using std::string;
using std::vector;

using Row = unsigned;
using Col = unsigned;
using Square = unsigned;
using Number = unsigned;

using RowCol = pair<Row, Col>;
using RowNum = pair<Row, Number>;
using ColNum = pair<Col, Number>;
using SqrNum = pair<Square, Number>;

using ArrayNumber = array<Number, 9>;
using ArrayRowCol = array<RowCol, 9>;

using MapRowColSquare = UnorderedMap<RowCol, Square>;
using MapRowColNumber = UnorderedMap<RowCol, Number>;

using MapRowBool = UnorderedMap<RowCol, bool>;
using MapColBool = UnorderedMap<RowCol, bool>;
using MapSquareBool = UnorderedMap<Square, bool>;

using MapRowColArrayNumber = UnorderedMap<RowCol, ArrayNumber>;

class Sudoku {
public:
  Sudoku(unsigned revealed);

private:
  void init();
  void ProcessAvailable();
  void SeedBoard(unsigned from, unsigned to);

  bool PlaceNumber();

  unsigned revealed_;

  const MapRowColSquare sqmap_;

  MapRowBool row_;
  MapColBool col_;
  MapSquareBool square_;

  MapRowCol board_;

  MapRowColArrayNumber available_;
};

}

#endif
