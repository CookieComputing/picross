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

  private def inBounds[A](tiles: IndexedSeq[A], index: Int): Boolean =
    0 <= index && index < tiles.size

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
   *
   * @param colored Whether or not the tile is colored
   * @return a freshly created tile
   */
  def newTile(colored: Boolean): Tile = colored

  /**
   * Retrieves the tile located at (row, col)
   *
   * @param posn the position of the tile
   * @param board Board The board to be checked
   * @return Some(tile) located at (row, col) for the given board, None if out of bounds
   */
  def tileAt(posn: Posn)(using board: Board): Option[Tile] = {
    val (row, col) = (posn.x, posn.y)
    if inBounds(board, row) && inBounds(board(row), col) then
      Some(board(row)(col)) else None
  }

  def getRow(row: Int)(using board: Board): Option[List[Tile]] =
    if inBounds(board, row) then Some(board(row).toList) else None

  def getCol(col: Int)(using board: Board): Option[List[Tile]] =
    // A board is guaranteed to have at least one element upon generation
    if inBounds(board(0), col) then
      Some((for {r <- board.indices} yield Board.tileAt(Posn(r, col))).toList.flatten)
    else None

  /**
   * Determines if the provided tile should be colored or not
   *
   * @param tile A tile to verify its coloring
   * @return Whether or not the tile is colored
   */
  def colored(tile: Tile): Boolean = tile

  /**
   * Returns the number of rows this board has
   * @param board the board to get rows from
   * @return the number of rows the board has
   */
  def numRows(board: Board): Int = board.size

  /**
   * Returns the number of cols this board has
   * @param board the board to get cols from
   * @return the number of cols the board has
   */
  def numCols(board: Board): Int = board(0).size
}