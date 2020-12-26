package day11

object Day11 {
  def main(args: Array[String]): Unit = {
    val input = 7400
    // 34,72
    println("Solving Day11A...")
    val resultA = solveA(input)
    println(resultA)
    // 233,187,13
    println("Solving Day11B...")
    val resultB = solveB(input)
    println(resultB)
  }

  final case class Pos(x: Int, y: Int)

  final case class Cell(pos: Pos, size: Int, power: Int)

  val SIZE = 300

  def solveA(serial: Int): Pos = {
    val limit = 3
    findMaxPowerSquare(limit, getPowerGrid(serial)).pos
  }

  // 0 based x & y needs to be translated to 1 based for calculations
  def calculateCellPower(serial: Int, col: Int, row: Int): Int = {
    val x = col + 1
    val y = row + 1
    val rackId = x + 10
    val level = (rackId * y + serial) * rackId
    val power =
      if (level < 100) 0
      else (level / 100) % 10
    power - 5
  }

  def getPowerGrid(serial: Int): Array[Array[Int]] = {
    val grid: Array[Array[Int]] = new Array[Array[Int]](SIZE)
    for (col <- 0 until SIZE) {
      grid(col) = new Array[Int](SIZE)
      for (row <- 0 until SIZE) {
        grid(col)(row) = calculateCellPower(serial, col, row)
      }
    }
    grid
  }

  def calculateWinPower(
      size: Int,
      gridWindowSizeOne: Array[Array[Int]],
      currentGrid: Array[Array[Int]],
      col: Int,
      row: Int
  ): Int = if (size == 1) {
    gridWindowSizeOne(col)(row)
  } else {
    val currentWindowPower = currentGrid(col)(row)
    val newCellsColPower =
      for (r <- row until (row + size))
        yield gridWindowSizeOne(col + size - 1)(r)
    // last col is -1 because the last cell has already been counted above
    val newCellsRowPower =
      for (c <- col until (col + size - 1))
        yield gridWindowSizeOne(c)(row + size - 1)
    currentWindowPower + newCellsColPower.sum + newCellsRowPower.sum
  }

  def findMaxPowerSquare(
      limit: Int,
      gridWindowSizeOne: Array[Array[Int]]
  ): Cell = {
    val currentGrid: Array[Array[Int]] = gridWindowSizeOne.map(_.map(identity))
    val currentMax = Cell(Pos(0, 0), size = 0, power = Int.MinValue)
    def go(size: Int, col: Int, row: Int, maxSoFar: Cell): Cell = {
      // because windows are squares
      val maxColOrRow = SIZE - size
      if (size > limit) {
        maxSoFar
      } else if (col > maxColOrRow) {
        go(size + 1, col = 0, row = 0, maxSoFar)
      } else if (row > maxColOrRow) {
        go(size, col + 1, row = 0, maxSoFar)
      } else {
        val windowPower =
          calculateWinPower(size, gridWindowSizeOne, currentGrid, col, row)
        currentGrid(col)(row) = windowPower
        val newMaxSoFar =
          if (windowPower <= maxSoFar.power) maxSoFar
          else {
            val x = col + 1
            val y = row + 1
            Cell(Pos(x, y), size, windowPower)
          }
        go(size, col, row + 1, newMaxSoFar)
      }
    }
    go(size = 1, col = 0, row = 0, maxSoFar = currentMax)
  }

  def solveB(serial: Int): (Int, Int, Int) = {
    val limit = SIZE
    val result = findMaxPowerSquare(limit, getPowerGrid(serial))
    (result.pos.x, result.pos.y, result.size)
  }
}
