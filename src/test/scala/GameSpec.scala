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
          case _: TileCross => passed
          case coloring: TileColor => {
            val origBoard = game.playerMarkedBoard
            assert(game.makeBoardMove(coloring).isDefined)
            val changedBoard = game.playerMarkedBoard
            assert(origBoard != changedBoard)
            assert(game.makeBoardMove(coloring).isDefined)
            val dupOfOrigBoard = game.playerMarkedBoard
            val newBoard = game.getSolution
            assert(origBoard == dupOfOrigBoard)
          }
      }
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
