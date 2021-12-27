import BoardSpec.*
import ClueSpec.exampleBoards
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.passed
import org.scalatest.FutureOutcome.failed
import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.TableFor3
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import picross.Board.{Board, Tile}
import picross.{Board, Clue}

class ClueSpec extends AnyPropSpec with ScalaCheckPropertyChecks with OptionValues {
  property("clues should pass the table tests for getClueForRow") {
    forAll(exampleBoards) {
      (tiles, rows, _) =>
        Board.newBoard(tiles.map(_.map(Board.newTile))) match
          case None => failed()
          case Some(board) =>
            assert(rows.zipWithIndex.forall(
              (row, index) => Clue.getClueForRow(index)(using board) match {
                case None => false
                case Some(Clue(clues)) => clues == row
              }))
    }
  }

  property("clues should pass the table tests for getClueForCol") {
    forAll(exampleBoards) {
      (tiles, _, cols) =>
        Board.newBoard(tiles.map(_.map(Board.newTile))) match
          case None => failed()
          case Some(board) =>
            assert(cols.zipWithIndex.forall(
              (col, index) => Clue.getClueForCol(index)(using board) match {
                case None => false
                case Some(Clue(clues)) => clues == col
              }))
    }
  }

  property("Every clues + the individual spaces between clues should be <= row/col size") {
    forAll(BoardSpec.validBoardGen) { board =>
      val rowSize = Board.numRows(board)
      val colSize = Board.numCols(board)
      assert((0 until rowSize).forall( row =>
        Clue.getClueForRow(row)(using board) match
          case None => false
          case Some(Clue(clues)) =>
            // -1 because we are counting the gaps between clues
            (clues.sum + clues.size - 1) <= colSize
      ))

      assert((0 until colSize).forall(col =>
        Clue.getClueForCol(col)(using board) match
          case None => false
          case Some(Clue(clues)) =>
            (clues.sum + clues.size - 1) <= rowSize
      ))
    }
  }

  property("The number of clues on a column or row is equal to the number of colored tiles") {
    forAll(sameSizedTiles) { tiles =>
      val coloredCount = tiles.map(_.count(Board.colored)).sum

      Board.newBoard(tiles) match
        case None => failed()
        case Some(board) =>
          val rowSize = Board.numRows(board)
          val colSize = Board.numCols(board)

          val colCount = (0 until colSize).foldLeft(0)((acc, col) => {
            acc + Clue.getClueForCol(col)(using board).value.clues.sum
          })

          val rowCount = (0 until rowSize).foldLeft(0)((acc, row) => {
            acc + Clue.getClueForRow(row)(using board).value.clues.sum
          })
          assert(rowCount == coloredCount && colCount == coloredCount)
    }
  }
}

object ClueSpec {
  val exampleBoards: TableFor3[List[List[Boolean]], List[List[Int]], List[List[Int]]] = Table(
    ("colors", "expected rows", "expected cols"),
    (List(
      List(true, true, false, true),
      List(true, true, true, true),
      List(false, false, false, true),
      List(false, true, false, true)
    ),
      List(
        List(2, 1),
        List(4),
        List(1),
        List(1, 1)),
      List(
        List(2),
        List(2, 1),
        List(1),
        List(4)
      )
    ),
    (
      List(
        List(false)
      ),
      List(List.empty[Int]),
      List(List.empty[Int])
    ),

    (
      List(
        List(true)
      ),
      List(
        List(1)
      ),
      List(
        List(1)
      )
    ),

    (
      List(
        List(true, true, true, true, true),
        List(true, false, false, false, true),
        List(true, false, false, false, true),
        List(true, false, false, false, true),
        List(true, true, true, true, true)),
      List(
        List(5),
        List(1, 1),
        List(1, 1),
        List(1, 1),
        List(5)
      ),
      List(
        List(5),
        List(1, 1),
        List(1, 1),
        List(1, 1),
        List(5)
      )
    ),

    (
      List(
        List(true, false, true, false),
        List(false, false, false, false),
        List(true, false, true, false),
        List(false, false, false, false)
      ),
      List(
        List(1, 1),
        List.empty[Int],
        List(1, 1),
        List.empty[Int]
      ),
      List(
        List(1, 1),
        List.empty[Int],
        List(1, 1),
        List.empty[Int]
      )
    ),
  )
}