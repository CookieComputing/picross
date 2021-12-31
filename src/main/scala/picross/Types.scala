package picross

/**
 * Data types representing the operations on a Picross board.
 */

enum BoardMove:
  // Mark a tile as colored
  case TileColor(posn: Posn)
  // Mark a tile as cleared.
  case TileCross(posn: Posn)

// rowClue determines whether the cross is applied to a row
// or a column, and the index represents the ith row/col.
case class ClueCross(rowClue: Boolean, index: Int, tileIndex: Int)