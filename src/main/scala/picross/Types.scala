package picross

/**
 * Data types representing the operations on a Picross board.
 */

enum BoardMove:
  // Mark a tile as colored
  case TileColor(posn: Posn)
  // Mark a tile as cleared. Automated suggests that the move was
  // automatically applied after all clues in the clue section were crossed off
  case TileCross(posn: Posn, automated: Boolean)

// rowClue determines whether the cross is applied to a row
// or a column, and the index represents the ith row/col.
case class ClueCross(rowClue: Boolean, index: Int, tileIndex: Int)