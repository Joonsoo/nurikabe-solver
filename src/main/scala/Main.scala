
// Created 2017-03-18 21:21 by Joonsoo Jeon

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Cell
sealed trait Occupiable extends Cell

case class Number(id: Int, count: Int) extends Cell
case object Wall extends Cell
case object Empty extends Cell with Occupiable
case object ShouldBeBelonged extends Cell with Occupiable
case class Belonged(to: Number) extends Cell
case object NotInterested extends Cell

class BoardUpdate(rows: Int, cols: Int) {
    private var _updates: Int = 0
    def updatesNumber: Int = _updates

    val cells: Seq[mutable.Buffer[Option[Cell]]] = (0 until rows) map { _ =>
        val array = new ArrayBuffer[Option[Cell]](cols)
        (0 until cols) foreach { _ => array += None }
        array
    }

    def apply(row: Int, col: Int): Option[Cell] =
        cells(row)(col)
    def update(pointer: (Int, Int), newCell: Cell): Unit = {
        if (!cells(pointer._1)(pointer._2).contains(newCell)) {
            _updates += 1
            cells(pointer._1)(pointer._2) = Some(newCell)
        }
    }
}

case class Line(cells: Seq[Cell]) {
    def apply(col: Int): Cell = cells(col)
}
case class Board(lines: Seq[Line]) {
    assert((lines map { _.cells.length }).toSet.size == 1)
    val rows: Int = lines.length
    val cols: Int = lines.head.cells.length
    val cells: Map[(Int, Int), Cell] = (lines.zipWithIndex flatMap { lineIdx =>
        val (line, row) = lineIdx
        line.cells.zipWithIndex map { cellIdx =>
            val (cell, col) = cellIdx
            (row, col) -> cell
        }
    }).toMap

    val numbers: Map[Number, (Int, Int)] =
        cells collect {
            case (rowCol, number: Number) =>
                number -> rowCol
        }
    val belongedMap: Map[Number, Set[(Int, Int)]] = numbers map { p =>
        val (number, pointer) = p
        number -> ((cells filter { _._2 == Belonged(number) }).keySet + pointer)
    }

    def print(): Unit = {
        val cellTextSize = (lines flatMap { line =>
            line.cells map {
                case Number(_, count) => count.toString.length
                case _ => 1
            }
        }).max
        lines foreach { line =>
            val cellString = line.cells map {
                case Number(_, count) => count.toString
                case Wall => "#"
                case Empty => "."
                case ShouldBeBelonged => "?"
                case Belonged(_) => "!"
                case NotInterested => "X"
            }
            println(cellString map { s => " " * (cellTextSize - s.length) + s } mkString " ")
        }
    }

    def printFriendlyCells(number: Number): Unit = {
        println(number)
        (0 until rows) foreach { row =>
            val friendly = (0 until cols) map { col =>
                adjacentFriendly(number, row, col)
            }
            println(friendly map { x => if (x) "O" else "." } mkString " ")
        }
    }
    def printReachableCells(number: Number): Unit = {
        println(number)
        val occs = occupationsSet(number)
        (0 until rows) foreach { row =>
            val reachable = (0 until cols) map { col =>
                occs.reachable(row, col)
            }
            println(reachable map { x => if (x) "O" else "." } mkString " ")
        }
    }

    def apply(row: Int, col: Int): Cell = lines(row)(col)
    def update(updates: BoardUpdate): Board = {
        Board(lines.zipWithIndex map { lineIdx =>
            val (line, row) = lineIdx
            val newLine = line.cells.zipWithIndex map { cellIdx =>
                val (cell, col) = cellIdx
                updates(row, col) match {
                    case Some(newCell) => newCell
                    case None => cell
                }
            }
            Line(newLine)
        })
    }

    def adjacentFriendly(number: Number, row: Int, col: Int): Boolean = {
        // row,col 주변 4개(모서리이면 3개나 2개)의 cell이 다른 number에 속하지 않으면(벽이거나 비어있으면) true
        (adjacentFrom(row, col) + ((row, col))) forall { p =>
            this(p._1, p._2) match {
                case r: Number => r == number
                case Belonged(r) => r == number
                case Wall => true
                case Empty => true
                case ShouldBeBelonged => true
                case NotInterested => true
            }
        }
    }
    def adjacentFrom(row: Int, col: Int): Set[(Int, Int)] = {
        Set((-1, 0), (1, 0), (0, -1), (0, 1)) map { p => (row + p._1, col + p._2) } filter { p => (p._1 >= 0) && (p._1 < rows) && (p._2 >= 0) && (p._2 < cols) }
    }
    def adjacent(points: Set[(Int, Int)]): Set[(Int, Int)] = {
        (points flatMap { p => adjacentFrom(p._1, p._2) }) -- points
    }
    def borderOf(cells: Set[(Int, Int)]): Set[(Int, Int)] = ???
    def occupationsSet(number: Number): OccupationsSet = {
        val occupied = belongedMap(number)
        // occupied는 모두 인접해있음

        // TODO friendly 캐싱
        // TODO 일단 무식하게 해놓고 필요하면 개선해야지..
        // TODO 일단 cells의 adjacentFriendly 구하는 부분을 재사용해서 줄일 수 있을 것 같다
        def rec(needed: Int, cells: Set[(Int, Int)]): Set[Occupation] =
            if (needed == 0) Set(Occupation(cells -- occupied)) else {
                val adjacents = adjacent(cells) filter { p => adjacentFriendly(number, p._1, p._2) }
                adjacents.foldLeft(Set[Occupation]()) { (cc, pointer) => cc ++ rec(needed - 1, cells + pointer) }
            }

        // occupied 주변으로 (number.count - occupied.size)만큼을 더 만들어야 한다
        val occupations: Set[Occupation] = rec(number.count - occupied.size, occupied)

        val (common, cands) = OccupationsSet.extractCommons(occupations)
        OccupationsSet(occupied, Occupation(common), cands)
    }

    def wallChunks: Set[WallChunk] = {
        ???
    }
    def emptyAdjacents(wallChunk: WallChunk): Set[(Int, Int)] = {
        ???
    }

    def applyOccupation(number: Number, common: Occupation, occupation: Occupation): Board = {
        // common과 occupation에서 지정한 것들을 적용한 새로운 보드를 반환한다
        ???
    }
    // TODO split하는 기능 추가

    def reachableNumbersTo(row: Int, col: Int): Seq[Number] = ???

    def isSolved: Boolean = ???
    def isFailed: Boolean = ???
}
object Board {
    def fromString(lines: Seq[String]): Board = {
        var numberId: Int = 0
        val cellLines = lines map { line =>
            val splitted = line.split("\\s+")
            val cells: Seq[Cell] = splitted map {
                case "." => Empty
                case n =>
                    numberId += 1
                    Number(numberId, n.toInt)
            }
            Line(cells)
        }
        Board(cellLines)
    }
}

case class Occupation(belonged: Set[(Int, Int)])
case class OccupationsSet(occupied: Set[(Int, Int)], common: Occupation, cands: Set[Occupation]) {
    // assert(cands forall { cand => (occupied.size + common.belonged.size + cand.belonged.size) == number.size })
    def reachable(row: Int, col: Int): Boolean = {
        val pointer = (row, col)
        (occupied contains pointer) || (common.belonged contains pointer) || (cands exists { _.belonged contains pointer })
    }
    def filterReachables(row: Int, col: Int): Option[OccupationsSet] = {
        // (row, col)을 포함하고 있는 경우에 포함하고 있는 occupation만 걸러서 반환하고, 그렇지 않으면 None을 반환
        val pointer = (row, col)
        if ((occupied contains pointer) || (common.belonged contains pointer)) {
            Some(this)
        } else {
            val filteredCands = cands filter { _.belonged contains pointer }
            if (filteredCands.isEmpty) None else {
                // TODO filteredCands중 common한 부분이 생겼을 경우 고려
                val (newCommons, newCands) = OccupationsSet.extractCommons(filteredCands)
                Some(OccupationsSet(occupied, Occupation(common.belonged ++ newCommons), filteredCands ++ newCands))
            }
        }
    }
}
object OccupationsSet {
    def extractCommons(occupations: Set[Occupation]): (Set[(Int, Int)], Set[Occupation]) = {
        // cands 중 공통인 부분들을 common으로 분리
        // TODO
        val cellsMap = occupations flatMap { occ => occ.belonged map { belonged => occ -> belonged } } groupBy { _._2 } mapValues { _ map { _._1 } }
        val commonCells = (cellsMap filter { _._2.size == occupations.size }).keySet
        if (commonCells.isEmpty) {
            (Set(), occupations)
        } else {
            (commonCells, occupations map { occ => Occupation(occ.belonged -- commonCells) })
        }
    }
}
case class WallChunk(walls: Set[(Int, Int)])

class Solver(board: Board) {
    val numberOccs: Map[Number, OccupationsSet] = (board.numbers.keys map { number => number -> board.occupationsSet(number) }).toMap

    def fillObvious(): Board = {
        val updates = new BoardUpdate(board.rows, board.cols)

        def repeatUntilStable(): Unit = {
            val initialUpdateNumber = updates.updatesNumber
            // Number에서 도달 불가능한 cell들의 위치 계산
            val unreachableEmptyCells: Set[(Int, Int)] = board.cells.keySet filter { p => !(numberOccs exists { _._2.reachable(p._1, p._2) }) }
            println(s"unreachableEmptyCells: $unreachableEmptyCells")
            // 1. unreachableEmptyCells에 벽 설치
            unreachableEmptyCells foreach { unreachable =>
                updates(unreachable) = Wall
            }

            // TODO ㄱ자 모양이 된 경우 빈 한 칸을 ShouldBeBelonged로 fill

            // 각 Number의 OccupationsSet에서 common인 부분들 Belonged로 fill
            numberOccs foreach { pair =>
                // TODO border도 처리
                val (number, occ) = pair
                if (occ.common.belonged.nonEmpty) {
                    println(s"common: $number, ${occ.common.belonged}")
                }
                occ.common.belonged foreach { p => updates(p) = Belonged(number) }
            }

            // TODO 한 셀에 대해 Occupation을 주장하는 Number가 하나뿐인 cell을 Belonged로 fill

            // TODO ShouldBeBelonged인 셀에 도달 가능한 number가 하나뿐이면 Belonged로 fill.
            // TODO - 이 때 이 지점에 도달 가능한 OccupationsSet에서 common한 부분들 Belonged로 fill
            if (initialUpdateNumber != updates.updatesNumber) repeatUntilStable()
        }
        repeatUntilStable()

        board.update(updates)
    }

    def splitted(): Seq[Board] = {
        ???
    }

    def solve(): Solver = {
        ???
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val board = Board.fromString(Seq(
            ". 3 . . . . . 2 .",
            ". . . . 5 . . . .",
            ". . . . . . . . .",
            ". . . 2 . . . . 5",
            ". . . . 2 . . . .",
            ". . . . . . . . .",
            ". 4 . . . . . 1 .",
            "1 . . 2 . . 5 . .",
            ". . . . . . . . .",
            ". . . . 1 . . . .",
            "4 . . . . . . . .",
            ". . . . 4 . . . .",
            "5 . . . . . . . 2"
        ))
        board.print()
        //        val number = board.numbers.last
        //        println(number)
        //        board.printFriendlyCells(number._1)
        //        board.printReachableCells(number._1)
        val afterBoard = new Solver(board).fillObvious()
        afterBoard.print()
    }
}
