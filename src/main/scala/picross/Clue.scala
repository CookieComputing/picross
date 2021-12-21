package picross

import picross.Board.{Board, Tile}

/**
 * Clues are a hint to the player that represent the number of tiles that are connected consecutively.
 * For instance, a "4" in a clue list indicates there are 4 tiles connected in a row. Elements that are separated
 * within the clues list are separated by AT LEAST one tile between them.
 *
 * @param clues The list of clues for a row or column. Refer to the class documentation for an interpretation of a
 *              clue list.
 */
case class Clue(clues: List[Int])

object Clue {
  /**
   * Retrieves clues for the provided column and board.
   * @param col the requested column number
   * @return Some(Clue) if col is a valid column number, None otherw ise
   */
  def getClueForCol(col: Int)(using Board): Option[Clue] = ???

  /**
   * Retrieves clues for the provided row and board.
   * @param row the requested column number
   * @return Some(Clue) if row is a valid column number, None otherw ise
   */
  def getClueForRow(row: Int)(using Board): Option[Clue] = ???

  private def getClueForList(tiles: List[Tile]): Clue = ???
}