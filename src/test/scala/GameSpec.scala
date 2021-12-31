import BoardSpec.*
import GameSpec.{maybeCrossMove, playerMoveGen, validGameGen, validPlayerBoardMoveGen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.passed
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import picross.*
import picross.Board.Board
import picross.BoardMove.*
import picross.Clue.{getClueForCol, getClueForRow}
import picross.Game.PlayerTile.Blank
import picross.Game.{PlayerMove, PlayerTile}

import scala.util.Random

class GameSpec extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("applying a move should change the board, while " +
    "re-applying the same move to a board should cause no effect") {
    forAll(validGameGen) { (game: Game) =>
      forAll(validPlayerBoardMoveGen(game.getSolution)) { (move: BoardMove) =>
        val Posn(row, col) = move match
          case TileColor(posn) => posn
          case TileCross(posn) => posn

        val origColor = game.playerMarkedBoard(row)(col)

        assert(game.makeBoardMove(move).isDefined)
        val changedBoard = game.playerMarkedBoard
        val changedColor = changedBoard(row)(col)
        assert(origColor != changedColor)

        assert(game.makeBoardMove(move).isDefined)
        val dupOfOrigColor = game.playerMarkedBoard(row)(col)
        if origColor == Blank then
          assert(origColor == dupOfOrigColor)
        else
          (move, origColor) match
            case (_: TileCross, PlayerTile.Cross) => assert(origColor == dupOfOrigColor)
            case (_: TileColor, PlayerTile.Color) => assert(origColor == dupOfOrigColor)
            case _ => assert(origColor != dupOfOrigColor)
      }
    }
  }

  property("Applying a single move should only change one entity") {
    forAll(validGameGen) { (game: Game) =>
      forAll(validPlayerBoardMoveGen(game.getSolution)) { (move: BoardMove) => {
        val origBoard = game.playerMarkedBoard
        val solution = game.getSolution

        def assertSameForAllBut(newBoard: IndexedSeq[IndexedSeq[PlayerTile]], posn: Posn): Unit = {
          val Posn(row, col) = posn
          for {
            r <- 0 until Board.numRows(solution)
            c <- 0 until Board.numCols(solution)
            if r != row
            if c != col
          } do {
            assert(newBoard(r)(c) == origBoard(r)(c))
          }
          assert(newBoard(row)(col) != origBoard(row)(col))
        }

        move match
          case TileCross(pos) =>
            val Posn(row, col) = pos
            assert(game.makeBoardMove(move).isDefined)

            val newBoard = game.playerMarkedBoard
            assertSameForAllBut(newBoard, pos)
            if origBoard(row)(col) == PlayerTile.Cross then
              assert(newBoard(row)(col) == PlayerTile.Blank)
            else
              assert(newBoard(row)(col) == PlayerTile.Cross)
          case TileColor(pos) =>
            val Posn(row, col) = pos
            assert(game.makeBoardMove(move).isDefined)

            val newBoard = game.playerMarkedBoard
            assertSameForAllBut(newBoard, pos)
            if origBoard(row)(col) == PlayerTile.Color then
              assert(newBoard(row)(col) == PlayerTile.Blank)
            else
              assert(newBoard(row)(col) == PlayerTile.Color)
      }
      }
    }
  }

  property("the player marked board has the same dimensions as the" +
    "game board") {
    forAll(validGameGen) { (game: Game) => {
      val board = game.playerMarkedBoard
      val solution = game.getSolution
      assert(board.size == Board.numRows(solution) &&
        board(0).size == Board.numCols(solution))
    }
    }
  }

  property("a game's history can be used to recreate the exact same " +
    "player marked board as the current game has") {
    forAll(validGameGen) { (origGame: Game) => {
      val newGame = Game(origGame.getSolution, origGame.history).value
      assert(origGame.playerMarkedBoard == newGame.playerMarkedBoard)
    }
    }
  }

  property("applying a move to a game should increase the history's size by one") {
    forAll(validGameGen) { (game: Game) => {
      var historyAdd = 0
      val origHistorySize = game.history.size
      forAll(validPlayerBoardMoveGen(game.getSolution)) { (move: BoardMove) => {
        assert(game.makeBoardMove(move).isDefined)
        historyAdd += 1
        assert(game.history.size == origHistorySize + historyAdd)
      }
      }
    }
    }
  }

  property("given n moves to apply to a game, after applying the moves, a game's history" +
    "should be used to fully reimplement the game") {
    forAll(validGameGen) { (game: Game) => {
      forAll(for {
        n <- Gen.chooseNum(0, 100)
        moves <- Gen.listOfN(n, playerMoveGen(game.getSolution))
      } yield moves) { (moves: List[BoardMove | Option[ClueCross]]) => {
        def applyMove(move: BoardMove | Option[ClueCross]) = move match
          case b: BoardMove => game.makeBoardMove(b)
          case Some(c: ClueCross) => game.makeClueMove(c)
          case None => Some(())

        moves.foreach(applyMove)

        val newGame = Game(game.getSolution, game.history).value
        assert(game.playerMarkedBoard == newGame.playerMarkedBoard)
        assert(game.getColCrosses == newGame.getColCrosses && game.getRowCrosses == newGame.getRowCrosses)
      }
      }
    }
    }
  }

  property("Undoing a move should result in the same game board as the board prior to move application") {
    forAll(validGameGen) { (game: Game) => {
      forAll(playerMoveGen(game.getSolution)) { (move: BoardMove | Option[ClueCross]) => {
        val origBoard = game.playerMarkedBoard
        val origRows = game.getRowCrosses
        val origCols = game.getColCrosses
        move match
          case b: BoardMove => game.makeBoardMove(b)
          case Some(c: ClueCross) => game.makeClueMove(c)
          case None => ()

        move match
          case None => ()
          case _ => game.undo()
        assert(origBoard == game.playerMarkedBoard)
        assert(origRows == game.getRowCrosses)
        assert(origCols == game.getColCrosses)
      }
      }
    }
    }
  }

  property("undoing an entire move history should result in a blank board and blank row/col cross list") {
    forAll(validGameGen) { (game: Game) => {
      game.history.foreach(_ => game.undo())

      assert(game.playerMarkedBoard.forall(_.forall(_ == Blank)))
      assert(game.getRowCrosses.forall(_.forall(_ == false)))
      assert(game.getColCrosses.forall(_.forall(_ == false)))
    }
    }
  }

  property("undoing past the move histories should result in a no-op") {
    forAll(validGameGen) { (game: Game) => {
      game.history.foreach(_ => game.undo())
      game.undo()

      assert(game.playerMarkedBoard.forall(_.forall(_ == Blank)))
      assert(game.getRowCrosses.forall(_.forall(_ == false)))
      assert(game.getColCrosses.forall(_.forall(_ == false)))
    }
    }
  }

  property("a game is considered completed if and only if just player " +
    "marked board's entries are indicated as true for all " +
    "colored tiles") { (game: Game) => {
    val board = game.playerMarkedBoard
    val solution = game.getSolution

    given Board = solution

    def completed(board: IndexedSeq[IndexedSeq[PlayerTile]]): Boolean = {
      (0 until Board.numRows(solution)).forall(row =>
        (0 until Board.numCols(solution)).forall(col =>
          Board.tileAt(Posn(row, col)).exists(Board.colored(_) == (board(row)(col) == PlayerTile.Color))
        )
      )
    }

    if !completed(board) then
      assert(!game.completed)
      for {
        r <- 0 until Board.numRows(solution)
        c <- 0 until Board.numCols(solution)
      } do {
        Board.tileAt(Posn(r, c)).foreach(tile => {
          (Board.colored(tile), board(r)(c)) match
            case (true, PlayerTile.Color) => ()
            case (false, PlayerTile.Blank) => ()
            case (false, PlayerTile.Cross) => ()
            case _ => game.makeBoardMove(TileColor(Posn(r, c)))
        })
      }
      val newBoard = game.playerMarkedBoard
      assert(completed(newBoard))
      assert(game.completed)
  }
  }

  property("applying a cross move on the board should apply the opposite state to the provided tile") {
    forAll(validGameGen) { (game: Game) =>
      forAll(maybeCrossMove(game.getSolution)) { (cross: Option[ClueCross]) => {
        cross match
          case None => passed
          case Some(cross) => {
            given Board = game.getSolution

            def getCurrVal = () => if cross.rowClue then
              game.getRowCrosses(cross.index)(cross.tileIndex)
            else
              game.getColCrosses(cross.index)(cross.tileIndex)

            val origVal = getCurrVal()
            game.makeClueMove(cross)
            val newVal = getCurrVal()
            assert(origVal != newVal)
          }
      }
      }
    }
  }
}

object GameSpec {
  val validPlayerBoardMoveGen: Board => Gen[BoardMove] = board => for {
    row <- Gen.chooseNum(0, Board.numRows(board) - 1)
    col <- Gen.chooseNum(0, Board.numCols(board) - 1)
    genFunc <- Gen.oneOf(List(TileColor(_), TileCross(_)))
  } yield genFunc(Posn(row, col))

  val maybeCrossMove: Board => Gen[Option[ClueCross]] = board => for {
    rowCross <- arbitrary[Boolean]
    index <- Gen.chooseNum(0, (if rowCross then Board.numRows(board) else Board.numCols(board)) - 1)
  } yield {
    given Board = board

    val clues = if rowCross then getClueForRow(index) else getClueForCol(index)
    clues.flatMap(c => {
      if c.clues.nonEmpty then Some(ClueCross(rowCross, index, Random.nextInt(c.clues.size)))
      else None
    })
  }

  val playerMoveGen: Board => Gen[BoardMove | Option[ClueCross]] = board => for {
    boardMove <- validPlayerBoardMoveGen(board)
    clueMove <- maybeCrossMove(board)
    move <- Gen.oneOf(List[BoardMove | Option[ClueCross]](boardMove, clueMove))
  } yield move

  val validGameGen: Gen[Game] = for {
    board <- validBoardGen
    historySize <- Gen.chooseNum(0, 100)
    history <- Gen.listOfN(historySize, validPlayerBoardMoveGen(board))
  } yield Game(board, history).value
}
