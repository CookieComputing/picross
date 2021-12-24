import BoardSpec.*
import GameSpec.{validGameGen, validPlayerBoardMoveGen}
import org.scalacheck.Gen
import org.scalacheck.Prop.passed
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import picross.Board.Board
import picross.{Board, BoardMove, Game, Posn}
import picross.BoardMove.*
import picross.Game.PlayerMove

class GameSpec extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("applying a move should change the board, while " +
    "re-applying the same move to a board should cause no effect") {
    forAll(validGameGen) { (game: Game) =>
      forAll(validPlayerBoardMoveGen(game.getSolution)) { (move: BoardMove) =>
        move match
          // TODO add cross support
          case _: TileCross => passed
          case coloring: TileColor => {
            val TileColor(Posn(row, col)) = coloring

            val origBoard = game.playerMarkedBoard
            val origColor = origBoard(row)(col)

            assert(game.makeBoardMove(coloring).isDefined)
            val changedBoard = game.playerMarkedBoard
            val changedColor = changedBoard(row)(col)
            assert(origBoard != changedBoard)
            assert(origColor != changedColor)

            assert(game.makeBoardMove(coloring).isDefined)
            val dupOfOrigBoard = game.playerMarkedBoard
            val dupOfOrigColor = dupOfOrigBoard(row)(col)
            val newBoard = game.getSolution
            assert(origBoard == dupOfOrigBoard)
            assert(origColor == dupOfOrigColor)
          }
      }
    }
  }

  property("Applying a single move should only change one entity") {
    forAll(validGameGen) { (game: Game) =>
      forAll(validPlayerBoardMoveGen(game.getSolution)) { (move: BoardMove) => {
          val origBoard = game.playerMarkedBoard

          assert(game.makeBoardMove(move).isDefined)

          val solution = game.getSolution
          val newBoard = game.playerMarkedBoard
          move match
            // TODO: Add cross support
            case _ : TileCross => passed
            case TileColor(Posn(row, col)) => {
              for {
                r <- 0 until Board.numRows(solution)
                c <- 0 until Board.numCols(solution)
                if r != row
                if c != col
              } do {
                assert(newBoard(r)(c) == origBoard(r)(c))
              }
              assert(newBoard(row)(col) == !origBoard(row)(col))
            }
        }
      }
    }
  }

  property("the player marked board has the same dimensions as the" +
    "game board") { (game: Game) => {
      val board = game.playerMarkedBoard
      val solution = game.getSolution
      assert(board.size == Board.numRows(solution) &&
        board(0).size == Board.numCols(solution))
    }
  }

  property("a game's history can be used to recreate the exact same " +
    "player marked board as the current game has") { (origGame: Game) => {
      val newGame = Game(origGame.getSolution, origGame.history)
      assert(origGame.playerMarkedBoard == newGame.playerMarkedBoard)
    }
  }

  property("a game is considered completed if and only if just player " +
    "marked board's entries are indicated as true for all " +
    "colored tiles") { (game: Game) => {
      val board = game.playerMarkedBoard
      val solution = game.getSolution
      given Board = solution

      def completed(board: IndexedSeq[IndexedSeq[Boolean]]): Boolean = {
        (0 until Board.numRows(solution)).forall(row =>
          ((0 until Board.numCols(solution)).forall(col =>
            Board.tileAt(Posn(row, col)).exists(Board.colored(_) == board(row)(col))
          ))
        )
      }

      if (!completed(board)) then
        assert(!game.completed)
        for {
          r <- 0 until Board.numRows(solution)
            c <- 0 until Board.numCols(solution)
          } do {
            Board.tileAt(Posn(r, c)).map(tile => {
              (Board.colored(tile), board(r)(c)) match
                case (true, true) => ()
                case (false, false) => ()
                case _ => game.makeBoardMove(TileColor(Posn(r,c)))
            })
        }
        val newBoard = game.playerMarkedBoard
        assert(completed(newBoard))
        assert(game.completed)
       end if
    }
  }
}

object GameSpec {
  val validPlayerBoardMoveGen: Board => Gen[BoardMove] = board => for {
    row <- Gen.chooseNum(0, Board.numRows(board)-1)
    col <- Gen.chooseNum(0, Board.numCols(board)-1)
    genFunc <- Gen.oneOf(List(TileColor(_), TileCross(_, false)))
  } yield genFunc(Posn(row, col))

  val validGameGen: Gen[Game] = for {
    board <- validBoardGen
    historySize <- Gen.const(0)
    history <- Gen.listOfN(historySize, validPlayerBoardMoveGen(board))
  } yield Game(board, history)
}
