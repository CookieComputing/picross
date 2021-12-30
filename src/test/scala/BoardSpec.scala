import BoardSpec.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.passed
import org.scalatest.FutureOutcome.failed
import org.scalatest.OptionValues
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import picross.Board.{Board, Tile}
import picross.{Board, Posn}

import scala.math

class BoardSpec extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a board should be non-empty") {
    forAll(atLeastOneGapInTiles) { tiles =>
      assert(Board.newBoard(tiles).isEmpty)
    }
  }

  property("a board's rows should all be of equal length") {
    forAll(differentSizedTiles) { tiles =>
      assert(Board.newBoard(tiles).isEmpty)
    }

    forAll(sameSizedTiles) { tiles =>
      assert(Board.newBoard(tiles).isDefined)

      val firstRowSize = tiles.head.size
      assert(tiles.forall(_.size == firstRowSize))
    }
  }

  property("a board's rows and cols should not be empty") {
    forAll(sameSizedTiles) { tiles =>
      Board.newBoard(tiles) match
        case None => failed()
        case Some(board) =>
          val rowSize = Board.numRows(board)
          val colSize = Board.numCols(board)

          assert((0 until rowSize).forall(row => {
            Board.getRow(row)(using board) match
              case None => false
              case Some(list) => list.nonEmpty
          }
          ))

          assert((0 until colSize).forall(col => {
            Board.getCol(col)(using board) match
              case None => false
              case Some(list) => list.nonEmpty
          }
          ))
    }
  }

  property("a board's tiles should be appropriately colored based on what was created") {
    forAll(sameSizedTiles) { case tiles: List[List[Tile]] =>
      val board = Board.newBoard(tiles)

      board match
        case None => failed()
        case Some(b) =>
          assert(tiles.map(_.zipWithIndex).zipWithIndex.forall(
            (row, rowIndex) => row.forall(
              (tile, colIndex) => Board.tileAt(Posn(rowIndex, colIndex))(using b) match
                case None => false
                case Some(boardTile) => Board.colored(boardTile) == Board.colored(tile))))
    }
  }

  property("Calling getRow on a valid part of a board " +
    "should return the same tiles per row as the tiles used to generate the board") {
    forAll(sameSizedTiles) { tiles =>
      Board.newBoard(tiles) match
        case None => failed()
        case Some(board) =>
          assert((0 until tiles.size).forall(
            row => Board.getRow(row)(using board) match
              case None => false
              case Some(rowTiles) =>
                rowTiles == tiles(row)))
    }
  }

  property("Calling getCol on a valid part of a board " +
    "should return the same tiles per col as the tiles used to generate the board") {
    forAll(sameSizedTiles) { tiles =>
      Board.newBoard(tiles) match
        case None => failed()
        case Some(board) =>
          assert((0 until tiles(0).size).forall(
            col => Board.getCol(col)(using board) match
              case None => false
              case Some(colTiles) =>
                colTiles == (for {r <- 0 until tiles.size} yield tiles(r)(col)).toList))
    }
  }

  property("a board should have > 0 rows and cols") {
    forAll(validBoardGen) { board =>
      assert(Board.numRows(board) > 0 && Board.numCols(board) > 0)
    }
  }

  property("a board's rows and cols should match the provided tiles") {
    forAll(sameSizedTiles) { tiles =>
      val rowSize = tiles.size
      val colSize = tiles(0).size

      Board.newBoard(tiles) match
        case None => failed()
        case Some(board) =>
          assert(Board.numRows(board) == rowSize && Board.numCols(board) == colSize)
    }
  }
}

object BoardSpec extends OptionValues {
  private val tileRowGen: Int => Gen[List[Tile]] = rowSize =>
    Gen.listOfN(rowSize, Gen.oneOf(Board.newTile(true), Board.newTile(false)))

  val tileRowMaybeGap: Gen[List[Tile]] = for {
    rowSize <- Gen.chooseNum(0, 100)
    tiles <- tileRowGen(rowSize)
  } yield tiles

  val tileRowNoGap: Gen[List[Tile]] = for {
    rowSize <- Gen.chooseNum(1, 100)
    tiles <- tileRowGen(rowSize)
  } yield tiles

  val differentSizedTiles: Gen[List[List[Tile]]] = for {
    initialRowSize <- Gen.chooseNum(1, 100)
    differentRowSize <- Gen.chooseNum(1, 100) suchThat (n => n != initialRowSize)
    firstTiles <- tileRowGen(initialRowSize)
    secondTiles <- tileRowGen(differentRowSize)
  } yield firstTiles :: List(secondTiles)

  val sameSizedTiles: Gen[List[List[Tile]]] = for {
    rowSize <- Gen.chooseNum(1, 100)
    totalSize <- Gen.chooseNum(1, 100)
    tiles <- Gen.listOfN(totalSize, tileRowGen(rowSize))
  } yield tiles

  val tilesWithPotentialGaps: Gen[List[List[Tile]]] =
    for {
      totalSize <- Gen.chooseNum(0, 100)
      tiles <- Gen.listOfN(totalSize, tileRowMaybeGap)
    } yield tiles

  val atLeastOneGapInTiles: Gen[List[List[Tile]]] =
    for {
      tiles <- tilesWithPotentialGaps
      randomIndex <- Gen.chooseNum(0, math.max(0, tiles.size - 1))
    } yield
      val (front, back) = tiles.splitAt(randomIndex)
      front ++ List(List.empty[Tile]) ++ back

  val validBoardGen: Gen[Board] = for {
    tiles <- sameSizedTiles
  } yield Board.newBoard(tiles).value
}