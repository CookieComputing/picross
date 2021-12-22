package picross

/**
 * Board is an implementation of the Picross board, containing information about where tiles
 * should be filled
 */
object Board {
  // The board type
  opaque type Board = IndexedSeq[IndexedSeq[Tile]]
  // A Tile is colored if it is true, uncolored if it is not
  opaque type Tile = Boolean

  /**
   * Constructs a new board from the provided 2D list of tiles. Note that each list should
   * have equal length, otherwise this function will return None
   *
   * @param tiles A 2D list of tiles, where the position of each tile in a list is
   * @return A fresh board containing tiles from the 2D list of tiles provided
   */
  def newBoard(tiles: List[List[Tile]]): Option[Board] = tiles match
    case Nil => None
    case ts =>
      val firstLen = ts.head.size
      if firstLen == 0 || !ts.forall(_.size == firstLen)
      then None else Some(ts.map(_.toIndexedSeq).toIndexedSeq)

  /**
   * Creates a new tile
   * @param colored Whether or not the tile is colored
   * @return a freshly created tile
   */
  def newTile(colored: Boolean): Tile = colored

  /**
   * Retrieves the tile located at (row, col)
   *
   * @param row   The requested row
   * @param col   The requested col
   * @param Board The board to be checked
   * @return Some(tile) located at (row, col) for the given board, None if out of bounds
   */
  def tileAt(row: Int, col: Int)(using board: Board): Option[Tile] = {
    def inBounds[A](tiles: IndexedSeq[A], index: Int): Boolean =
      0 <= index && index < tiles.size

    if inBounds(board, row) && inBounds(board(row), col) then
      Some(board(row)(col)) else None
  }

  /**
   * Determines if the provided tile should be colored or not
   *
   * @param tile A tile to verify its coloring
   * @return Whether or not the tile is colored
   */
  def colored(tile: Tile): Boolean = tile
}