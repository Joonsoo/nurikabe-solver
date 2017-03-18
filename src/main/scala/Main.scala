
// Created 2017-03-18 21:21

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Cell
sealed trait Occupiable extends Cell

case class Number(row: Int, col: Int, count: Int) extends Cell
case object Wall extends Cell
case object Empty extends Cell with Occupiable
case object ShouldBeBelonged extends Cell with Occupiable
case class Belonged(to: Number) extends Cell
case object NotInterested extends Cell

class BoardUpdate(board: Board) {
    private var _updates: Int = 0
    def updatesNumber: Int = _updates

    val cells: Seq[mutable.Buffer[Option[Cell]]] = (0 until board.rows) map { _ =>
        val array = new ArrayBuffer[Option[Cell]](board.cols)
        (0 until board.cols) foreach { _ => array += None }
        array
    }

    def apply(row: Int, col: Int): Option[Cell] =
        cells(row)(col)
    def update(pointer: (Int, Int), newCell: Cell): Unit = {
        if (!cells(pointer._1)(pointer._2).contains(newCell) && board(pointer._1, pointer._2) != newCell) {
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

    def unsolvedNumbers: Set[Number] = numbers.keySet filter { number => belongedMap(number).size < number.count }

    def print(): Unit = {
        val cellTextSize = (lines flatMap { line =>
            line.cells map {
                case Number(_, _, count) => count.toString.length
                case _ => 1
            }
        }).max
        println(("=" * (cellTextSize + 1)) * cols)
        lines foreach { line =>
            val cellString = line.cells map {
                case Number(_, _, count) => count.toString
                case Wall => "#"
                case Empty => "."
                case ShouldBeBelonged => "?"
                case Belonged(_) => "_"
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

    def adjacentFrom(row: Int, col: Int): Set[(Int, Int)] = {
        Set((-1, 0), (1, 0), (0, -1), (0, 1)) map { p => (row + p._1, col + p._2) } filter { p => (p._1 >= 0) && (p._1 < rows) && (p._2 >= 0) && (p._2 < cols) }
    }
    def adjacentFriendly(number: Number, row: Int, col: Int): Boolean = {
        // row,col 주변 4개(모서리이면 3개나 2개)의 cell이 다른 number에 속하지 않으면(벽이거나 비어있으면) true
        this(row, col) match {
            case r: Number => false
            case Belonged(r) => r == number
            case Wall => false
            case _ =>
                adjacentFrom(row, col) forall { p =>
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
    }
    def adjacent(points: Set[(Int, Int)]): Set[(Int, Int)] = {
        (points flatMap { p => adjacentFrom(p._1, p._2) }) -- points
    }
    def borderOf(points: Set[(Int, Int)]): Set[(Int, Int)] = {
        // cells는 모두 인접해있어야 함
        adjacent(points)
    }
    def occupationsSet(number: Number): OccupationsSet = {
        val occupied = belongedMap(number)
        // occupied는 모두 인접해있음

        // TODO friendly 캐싱
        // TODO 일단 무식하게 해놓고 필요하면 개선해야지..
        // TODO 일단 cells의 adjacentFriendly 구하는 부분을 재사용해서 줄일 수 있을 것 같다
        def rec(needed: Int, cells: Set[(Int, Int)]): Set[Occupation] =
            if (needed == 0) Set(Occupation(cells -- occupied, borderOf(cells))) else {
                val adjacents = adjacent(cells) filter { p => adjacentFriendly(number, p._1, p._2) }
                adjacents.foldLeft(Set[Occupation]()) { (cc, pointer) => cc ++ rec(needed - 1, cells + pointer) }
            }

        // occupied 주변으로 (number.count - occupied.size)만큼을 더 만들어야 한다
        val occupations: Set[Occupation] = rec(number.count - occupied.size, occupied)

        val (common, cands) = OccupationsSet.extractCommons(occupations)
        OccupationsSet(occupied, common, cands).filterValid(this)
    }

    lazy val wallChunks: Set[WallChunk] = {
        var visited = Set[(Int, Int)]()

        def rec(queue: List[(Int, Int)], walls: Set[(Int, Int)]): WallChunk = {
            queue match {
                case (row, col) +: rest =>
                    val adjacentWalls = (adjacentFrom(row, col) filter { p => cells(p) == Wall }) -- walls -- queue
                    assert((adjacentWalls intersect visited).isEmpty)
                    visited ++= adjacentWalls
                    rec(adjacentWalls.toList ++ rest, walls ++ adjacentWalls)
                case List() =>
                    WallChunk(walls)
            }
        }
        (cells filter { _._2 == Wall }).foldLeft(Set[WallChunk]()) { (cc, cell) =>
            if (!(visited contains cell._1)) cc + rec(List(cell._1), Set(cell._1)) else cc
        }
    }
    def emptyAdjacents(wallChunk: WallChunk): Set[(Int, Int)] = {
        // wallChunk를 둘러싼 벽 중에 Empty만 반환
        adjacent(wallChunk.walls) filter { pointer => cells(pointer) == Empty }
    }

    // TODO split하는 기능 추가

    def isSolved: Boolean = {
        // 1. 빈칸이 없어야 하고
        val finished = cells.values forall { cell => (cell != Empty) && (cell != ShouldBeBelonged) && (cell != NotInterested) }

        // 2. wallChunks가 하나여야 하고
        val oneWallChunk = wallChunks.size == 1

        // 3. 2x2 wall이 없어야 하고
        val wallChunk = wallChunks.head
        val no22: Boolean = wallChunk.no22

        // 4. 각 Number가 차지한 영역이 정확해야 함
        val correctNumbers = numbers.keySet forall { number => belongedMap(number).size == number.count }

        finished && oneWallChunk && no22 && correctNumbers
    }

    def isFailed: Boolean = {
        val exists22 = wallChunks exists { wc => !wc.no22 }

        exists22
    }
}
object Board {
    def fromString(lines: Seq[String]): Board = {
        val cellLines = lines.zipWithIndex map { lineIdx =>
            val (line, row) = lineIdx
            val splitted = line.split("\\s+")
            val cells: Seq[Cell] = splitted.zipWithIndex map {
                case (".", _) => Empty
                case (n, col) =>
                    Number(row, col, n.toInt)
            }
            Line(cells)
        }
        Board(cellLines)
    }
}

case class Occupation(belonged: Set[(Int, Int)], border: Set[(Int, Int)]) {
    def +(other: Occupation): Occupation = Occupation(belonged ++ other.belonged, border ++ other.border)
    def toUpdates(number: Number, update: BoardUpdate): BoardUpdate = {
        belonged foreach { p => update(p) = Belonged(number) }
        border foreach { p => update(p) = Wall }
        update
    }
}
case class OccupationsSet(occupied: Set[(Int, Int)], common: Occupation, cands: Set[Occupation]) {
    // assert(cands forall { cand => (occupied.size + common.belonged.size + cand.belonged.size) == number.size })
    def reachable(row: Int, col: Int): Boolean = {
        val pointer = (row, col)
        (occupied contains pointer) || (common.belonged contains pointer) || (cands exists { _.belonged contains pointer })
    }
    def allReachables: Set[(Int, Int)] = {
        // 아직 확실치 않은 cell 중에 reachable들만 반환, occupied는 제외
        common.belonged ++ (cands flatMap { _.belonged })
    }
    def filterReachables(row: Int, col: Int): Option[OccupationsSet] = {
        // (row, col)을 포함하고 있는 경우에 포함하고 있는 occupation만 걸러서 반환하고, 그렇지 않으면 None을 반환
        val pointer = (row, col)
        if ((occupied contains pointer) || (common.belonged contains pointer)) {
            Some(this)
        } else {
            val filteredCands = cands filter { _.belonged contains pointer }
            if (filteredCands.isEmpty) None else {
                // filteredCands중 common한 부분이 생겼을 경우 고려
                val (newCommons, newCands) = OccupationsSet.extractCommons(filteredCands)
                Some(OccupationsSet(occupied, common + newCommons, filteredCands ++ newCands))
            }
        }
    }
    def filterValid(board: Board): OccupationsSet = {
        // cands중 인접하지 않은 Occupation을 만들어내는 것들을 걸러냄
        val validCands: Set[Occupation] = cands filter { cand =>
            val cells = occupied ++ common.belonged ++ cand.belonged
            cells forall { cell =>
                (board.adjacentFrom(cell._1, cell._2) intersect cells).nonEmpty
            }
        }
        if (validCands.size == cands.size) this else {
            val (newCommons, newCands) = OccupationsSet.extractCommons(validCands)
            OccupationsSet(occupied, common + newCommons, validCands ++ newCands)
        }
    }
}
object OccupationsSet {
    def extractCommons(occupations: Set[Occupation]): (Occupation, Set[Occupation]) = {
        // cands 중 공통인 부분들을 common으로 분리
        // TODO
        val cellsMap = occupations flatMap { occ => occ.belonged map { belonged => occ -> belonged } } groupBy { _._2 } mapValues { _ map { _._1 } }
        val commonCells = (cellsMap filter { _._2.size == occupations.size }).keySet
        val bordersMap = occupations flatMap { occ => occ.border map { border => occ -> border } } groupBy { _._2 } mapValues { _ map { _._1 } }
        val commonBorders = (bordersMap filter { _._2.size == occupations.size }).keySet
        if (commonCells.isEmpty && commonBorders.isEmpty) {
            (Occupation(Set(), Set()), occupations)
        } else {
            (Occupation(commonCells, commonBorders), occupations map { occ => Occupation(occ.belonged -- commonCells, occ.border -- commonBorders) })
        }
    }
}
case class WallChunk(walls: Set[(Int, Int)]) {
    def no22: Boolean =
        !(walls exists { w =>
            Set((1, 0), (0, 1), (1, 1)) forall { p => walls contains ((w._1 + p._1, w._2 + p._2)) }
        })
}

class Solver(board: Board) {
    lazy val numberOccs: Map[Number, OccupationsSet] =
        (board.numbers.keys map { number => number -> board.occupationsSet(number) }).toMap

    lazy val reachableNumbers: Map[(Int, Int), Set[Number]] =
        numberOccs.toSet flatMap { (kv: (Number, OccupationsSet)) =>
            kv._2.allReachables map { r => (kv._1, r) }
        } groupBy { _._2 } mapValues { x => x map { _._1 } }

    def fillObvious(): (Board, Boolean) = {
        val updates = new BoardUpdate(board)
        val initialUpdatesVersion = updates.updatesNumber

        // Number에서 도달 불가능한 cell들의 위치 계산
        val unreachableEmptyCells: Set[(Int, Int)] = board.cells.keySet filter { p => !(numberOccs exists { _._2.reachable(p._1, p._2) }) }
        //        println(s"unreachableEmptyCells: $unreachableEmptyCells")
        // 1. unreachableEmptyCells에 벽 설치
        unreachableEmptyCells foreach { unreachable =>
            updates(unreachable) = Wall
        }

        // 각 Number의 OccupationsSet에서 common인 부분들 Belonged나 Wall로 fill
        numberOccs foreach { pair =>
            val (number, occ) = pair
            if (occ.common.belonged.nonEmpty) {
                //                println(s"commonCells: $number, ${occ.common.belonged}")
                occ.common.belonged foreach { p => updates(p) = Belonged(number) }
            }
            if (occ.common.border.nonEmpty) {
                //                println(s"commonBorders: $number, ${occ.common.border}")
                occ.common.border foreach { p => updates(p) = Wall }
            }
        }

        // TODO 둘러싼 공간 중 Empty가 하나밖에 없는 WallChunk가 있으면 그 빈 칸에 Wall
        board.wallChunks foreach { wallChunk =>
            val empties = board.emptyAdjacents(wallChunk)
            if (empties.size == 1) {
                val empty = empties.head
                //                println(s"oneEmptyAdjacent: $empty")
                updates(empty) = Wall
            }
        }

        // TODO ㄱ자 모양의 벽이 있으면 빈 한 칸을 ShouldBeBelonged로 fill

        // ShouldBeBelonged인 셀에 도달 가능한 number가 하나뿐이면 Belonged로 fill.
        // TODO - 이 때 이 지점에 도달 가능한 OccupationsSet에서 common한 부분들 Belonged로 fill -> 필요한가?
        val shouldBeBelonged = board.cells filter { _._2 == ShouldBeBelonged }
        shouldBeBelonged foreach { pair =>
            val (pointer, _) = pair
            if (reachableNumbers(pointer).size == 1) {
                val number = reachableNumbers(pointer).head
                //                println(s"onlyReachableNumber: $pointer $number")
            }
        }

        (board.update(updates), updates.updatesNumber != initialUpdatesVersion)
    }

    def splitted(): Seq[Board] = {
        ???
    }

    def solve(): Solver = {
        ???
    }
}

object Main {
    val hard22: Board = Board.fromString(Seq(
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
    val hard23: Board = Board.fromString(Seq(
        ". . . . 5 . . . .",
        ". . . . . . . . 2",
        ". . 2 . . . . . .",
        "7 . . . . . . . .",
        ". . 2 . . . . 6 .",
        ". . . . . . 2 . .",
        ". 2 . . . . . . .",
        ". . . 4 . . . . .",
        "4 . . . . . . . 2",
        ". . . . . . . . .",
        ". . . . . . 4 . .",
        "3 . 2 . . 3 . . .",
        ". . . 1 . . . 4 ."
    ))
    val normal14: Board = Board.fromString(Seq(
        ". . . 4 . 5 .",
        ". . . . . . .",
        ". . 2 . . . .",
        ". . . . . 3 .",
        ". . . 3 . . .",
        ". . . . . . .",
        ". . 2 . . . .",
        ". . . 6 . . .",
        ". . 1 . . . .",
        "2 . . . . 1 ."
    ))

    def main(args: Array[String]): Unit = {
        val board = hard23
        board.print()
        //        val number = board.numbers.last
        //        println(number)
        //        board.printFriendlyCells(number._1)
        //        board.printReachableCells(number._1)

        def trySolve(board: Board): (Board, Boolean) = {
            val solver = new Solver(board)

            //            println("*************************")
            //            println(solver.numberOccs(Number(12, 0, 5)))
            //            println(board(9, 1))
            //            println(solver.numberOccs(Number(10, 0, 4)))
            //            println()

            val (afterBoard, updated) = solver.fillObvious()
            //            afterBoard.print()
            //            println(s"updated: $updated")
            //            println(s"wallChunks: ${board.wallChunks}")
            (afterBoard, updated)
        }

        def solveUntilStable(board: Board): Board = {
            val (newBoard, updated) = trySolve(board)
            if (updated) solveUntilStable(newBoard) else newBoard
        }

        val lastBoard = solveUntilStable(board)
        println("Obvious Filled:")
        lastBoard.print()

        def bruteforce(board: Board, trace: List[String]): Unit = {
            println(trace mkString " <- ")
            if (board.isSolved) {
                board.print()
                println("Solved!")
                System.exit(0)
            } else {
                val unsolvedNumbers = board.unsolvedNumbers.toSeq
                val occs = unsolvedNumbers map { number => number -> board.occupationsSet(number) } sortBy { x => x._2.cands.size }

                occs foreach { numberOccs =>
                    val (number, occupationsSet) = numberOccs
                    val cands = occupationsSet.cands.toSeq

                    def tryOccupation(occupation: Occupation, traceText: String): Unit = {
                        val board1 = board.update(occupation.toUpdates(number, new BoardUpdate(board)))
                        val board2 = solveUntilStable(board1)
                        if (!board2.isFailed) {
                            board2.print()
                            bruteforce(board2, traceText +: trace)
                        }
                    }

                    cands.zipWithIndex foreach { candIdx =>
                        val (cand, idx) = candIdx
                        tryOccupation(cand, s"$number($idx/${cands.size})")
                    }
                }
            }
        }
        bruteforce(lastBoard, List())
    }
}
