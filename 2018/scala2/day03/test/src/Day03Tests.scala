package day03

import utest._
import Day03.{parseClaim, explodeClaim, Claim, Pos}

object Day03Tests extends TestSuite {
  def tests = Tests {
    test("parseClaim") {
      test("#3 @ 5,5: 2x2") {
        val input = "#3 @ 5,5: 2x2"
        val result = parseClaim(input)
        val expected = Claim(3, Pos(5, 5), 2, 2)
        assert(result == expected)
      }
      test("#1225 @ 367,185: 22x12") {
        val input = "#1225 @ 367,185: 22x12"
        val result = parseClaim(input)
        val expected = Claim(1225, Pos(367, 185), 22, 12)
        assert(result == expected)
      }
    }
    test("explodeClaim") {
      test("#3 @ 5,5: 2x3") {
        val input = Claim(3, Pos(5, 5), 2, 3)
        val result = explodeClaim(input)
        val expected = List(
          Claim(3, Pos(5, 5), 1, 1),
          Claim(3, Pos(5, 6), 1, 1),
          Claim(3, Pos(5, 7), 1, 1),
          Claim(3, Pos(6, 5), 1, 1),
          Claim(3, Pos(6, 6), 1, 1),
          Claim(3, Pos(6, 7), 1, 1)
        )
        assert(result.length == 6)
        assert(result == expected)
      }
    }
  }
}
