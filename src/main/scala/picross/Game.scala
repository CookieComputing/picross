package picross

import picross.Board.{Board, tileAt}
import picross.BoardMove.*
import picross.ClueCross
import picross.Game.PlayerTile.*
import picross.Game.{PlayerMove, PlayerTile}

/**
 * A Game is an instance of a Picross game, containing information about the board and what the player
 * has marked as, as well as a history of the past moves that the Game has seen previously.
 */
class Game(solution: Board,
           private val givenMoveHistory: List[PlayerMove],
           private val rowClues: IndexedSeq[Clue],
           private val colClues: IndexedSeq[Clue]) {
  // Game is one of the few locations in the code that has mutation
  // for the sake of efficiency
  given Board = solution

  private val internalBoard = Array.tabulate(Board.numRows(solution))(_ =>
    Array.tabulate(Board.numCols(solution))(_ => Blank))
  // Optimization to enable fast completion checking for after game render
  private var correct = 0
  private val solutionCorrect = (0 until Board.numRows(solution)).map(row =>
    (0 until Board.numCols(solution)).count(col =>
      Board.tileAt(Posn(row, col)).exists(Board.colored))
  ).sum

  private val rowCross = rowClues.map(_.clues.map(_ > 0).toArray)
  private val colCross = colClues.map(_.clues.map(_ > 0).toArray)

  private var moveHistory = List.empty[PlayerMove]
  givenMoveHistory.foldRight(())((move, _) => move match
    case c: ClueCross => makeClueMove(c)
    case b: BoardMove => makeBoardMove(b)
  )


  /**
   * Determine if the game is complete.
   *
   * @return
   */
  def completed: Boolean = correct == solutionCorrect

  /**
   * Returns a history of the player's past moves.
   *
   * @return a list of moves
   */
  def history: List[PlayerMove] = moveHistory

  /**
   * Performs the move action requested on the board.
   *
   * @param move the move to perform
   * @return Some(()) if successful, None if not
   */
  def makeBoardMove(move: BoardMove): Option[Unit] =
    (move match
      case TileColor(pos) => tileAt(pos).map(_ => {
        val Posn(row, col) = pos
        val prev = internalBoard(row)(col)
        updateCorrectnessTile(if prev == Color then Blank else Color, pos)
      })
      case TileCross(pos, _) => tileAt(pos).map(_ => {
        val Posn(row, col) = pos
        val prev = internalBoard(row)(col)
        updateCorrectnessTile(if prev == Cross then Blank else Cross, pos)
      })).map(_ => {
      moveHistory = move :: moveHistory
    })

  private def inBounds[A](xs: Seq[A], index: Int): Boolean = 0 <= index && index < xs.size

  // If a tile should be colored but is changed, decrement correct tile
  private def updateCorrectnessTile(newTile: PlayerTile, pos: Posn): Option[Unit] =
    val Posn(row, col) = pos
    if !(inBounds(internalBoard, row) && inBounds(internalBoard(row), col)) then
      None
    else {
      if internalBoard(row)(col) == Color then
        correct -= 1
      else if internalBoard(row)(col) != Color && Board.tileAt(pos).exists(Board.colored) then
        correct += 1
      Some(internalBoard(row).update(col, newTile))
    }

  /**
   * Performs a clue based move.
   *
   * @param move the move to perform
   * @return Some(()) if success, None if not
   */
  def makeClueMove(move: ClueCross): Option[Unit] = {
    val crosses = if move.rowClue then rowCross else colCross
    if !(inBounds(crosses, move.index) && inBounds(crosses(move.index), move.tileIndex)) then
      None
    else
      val xs = crosses(move.index)
      moveHistory = move :: moveHistory
      Some(xs.update(move.tileIndex, !xs(move.tileIndex)))
  }

  /**
   * Get player crosses for the rows
   *
   * @return row crosses
   */
  def getRowCrosses: List[List[Boolean]] = rowCross.map(_.toList).toList

  /**
   * Get player crosses for the columns
   *
   * @return col crosses
   */
  def getColCrosses: List[List[Boolean]] = colCross.map(_.toList).toList

  /**
   * Returns the player's marked board
   *
   * @return the player's marked board, true indicates that the player has marked the board, while false means there is
   *         no marking
   */
  def playerMarkedBoard: IndexedSeq[IndexedSeq[PlayerTile]] = internalBoard.map(_.toIndexedSeq).toIndexedSeq

  def getSolution: Board = solution
}

object Game {
  type PlayerMove = BoardMove | ClueCross
  type Colored = Boolean

  enum PlayerTile:
    case Color, Blank, Cross

  /**
   * Creates a brand new game instance
   *
   * @return a new game
   */
  def apply(board: Board): Option[Game] = Game(board, List.empty[PlayerMove])

  /**
   * Creates a new Game and applies all past moves to the board
   *
   * @param history A list of moves that have occurred in the past
   * @return a new game with a potential list of moves applied
   */
  def apply(board: Board, history: List[PlayerMove]): Option[Game] = {
    given Board = board

    val rowClues = (0 until Board.numRows(board)).flatMap(Clue.getClueForRow)
    val colClues = (0 until Board.numCols(board)).flatMap(Clue.getClueForCol)
    Some(new Game(board, history, rowClues, colClues))
  }
}