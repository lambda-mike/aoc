package day11

import utest._
import Day11._

object Day11Tests extends TestSuite {
  def tests = Tests {
    test("calculateCellPower") {
      test("8 3,5") {
        val serial = 8
        val result = calculateCellPower(serial, 2, 4)
        val expected = 4
        assert(result == expected)
      }
      test("57 122,79") {
        val serial = 57
        val result = calculateCellPower(serial, 121, 78)
        val expected = -5
        assert(result == expected)
      }
      test("39 217,196") {
        val serial = 39
        val result = calculateCellPower(serial, 216, 195)
        val expected = 0
        assert(result == expected)
      }
      test("71 101,153") {
        val serial = 71
        val result = calculateCellPower(serial, 100, 152)
        val expected = 4
        assert(result == expected)
      }
    }
    test("solveA 3") {
      test("18") {
        val serial = 18
        val result = solveA(serial)
        val expected = Pos(33, 45)
        assert(result == expected)
      }
      test("42") {
        val serial = 42
        val result = solveA(serial)
        val expected = Pos(21, 61)
        assert(result == expected)
      }
    }
    // test("solveB") {
    //   test("18") {
    //     val serial = 18
    //     val result = solveB(serial)
    //     val expected = Cell(Pos(90, 26), 16, 113)
    //     assert(result == expected)
    //   }
    //   test("42") {
    //     val serial = 42
    //     val result = solveB(serial)
    //     val expected = Cell(Pos(232, 251), 12, 119)
    //     assert(result == expected)
    //   }
    // }
  }
}
