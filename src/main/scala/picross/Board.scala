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
   * @param tiles A 2D list of tiles, where the position of each tile in a list is
   * @return A fresh board containing tiles from the 2D list of tiles provided
   */
  def newBoard(tiles: List[List[Tile]]): Option[Board] = ???

  /**
   * Retrieves the tile
   * @param row
   * @param col
   * @param Board
   * @return
   */
  def apply(row: Int, col: Int)(using Board): Option[Tile] = ???

  /**
   * Determines if the provided tile should be colored or not
   * @param tile A tile to verify its coloring
   * @return Whether or not the tile is colored
   */
  def colored(tile: Tile): Boolean = ???
}