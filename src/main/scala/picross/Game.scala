package picross

import picross.Board.{Board, tileAt}
import picross.Game.PlayerMove
import picross.ClueCross
import picross.BoardMove.*

/**
 * A Game is an instance of a Picross game, containing information about the board and what the player
 * has marked as, as well as a history of the past moves that the Game has seen previously.
 * @param internalBoard an internal representation of what the player has colored on the board
 */
class Game(solution: Board,
           private var moveHistory: List[PlayerMove]) {
  given Board = solution
  private val internalBoard = Array.tabulate(Board.numRows(solution))(_ =>
    Array.tabulate(Board.numCols(solution))(_ => false))

  moveHistory.foldRight(())((move, _) => move match
    case c: ClueCross => makeClueMove(c)
    case b: BoardMove => makeBoardMove(b)
  )

  /**
   * Determine if the game is complete.
   * @return
   */
  def completed: Boolean =
    (0 until Board.numRows(solution)).forall( row =>
      (0 until Board.numCols(solution)).forall( col =>
        Board.tileAt(Posn(row, col)).exists(Board.colored) == internalBoard(row)(col)
      )
    )

  /**
   * Returns a history of the player's past moves.
   * @return a list of moves
   */
  def history: List[PlayerMove] = moveHistory

  /**
   * Performs the move action requested on the board.
   * @param move the move to perform
   * @return Some(()) if successful, None if not
   */
  def makeBoardMove(move: BoardMove): Option[Unit] = move match
    case TileColor(pos) => tileAt(pos).map(_ => {
      val Posn(row, col) = pos
      val prev = internalBoard(row)(col)
      Some(internalBoard(row).update(col, !prev))
      // TODO: Strip out any tile crosses on that tile
    })
    case TileCross(pos, _) => tileAt(pos).map(_ => {
      val Posn(row, col) = pos
      Some(internalBoard(row).update(col, false))

      // TODO: Add support for crosses
    })

  /**
   * Performs a clue based move.
   * @param move the move to perform
   * @return Some(()) if success, None if not
   */
  def makeClueMove(move: ClueCross): Option[Unit] = None
  // TODO: implement

  /**
   * Returns the player's marked board
   * @return the player's marked board, true indicates that the player has marked the board, while false means there is
   *         no marking
   */
  def playerMarkedBoard: IndexedSeq[IndexedSeq[Boolean]] = internalBoard.map(_.toIndexedSeq).toIndexedSeq

  def getSolution: Board = solution
}

object Game {
  type PlayerMove = BoardMove | ClueCross
  type Colored = Boolean
  /**
   * Creates a brand new game instance
   * @return a new game
   */
  def apply(board: Board): Game = Game(board, List.empty[PlayerMove])

  /**
   * Creates a new Game and applies all past moves to the board
   * @param history A list of moves that have occurred in the past
   * @return a new game with a potential list of moves applied
   */
  def apply(board: Board, history: List[PlayerMove]): Game = new Game(board, history)
}
