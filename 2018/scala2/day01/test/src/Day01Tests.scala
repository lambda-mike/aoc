package day01

import utest._

object Day01Tests extends TestSuite {
  def tests = Tests {
    test("parseFreqChanges") {
      val lines = List("+1", "-2", "+3", "+1")
      val result = Day01.parseFreqChanges(lines)
      assert(result == List(1, -2, 3, 1))
    }
    test("sumFreqChanges") {
      val freqs = List(1, -2, 3, 1)
      val result = Day01.sumFreqChanges(freqs)
      assert(result == 3)
    }
    test("detectDuplicateFreq") {
      test("sample 0") {
        val freqs = List(1, -2, 3, 1)
        val result = Day01.detectDuplicateFreq(freqs)
        assert(result == 2)
      }
      test("sample 1") {
        val freqs = List(1, -1)
        val result = Day01.detectDuplicateFreq(freqs)
        assert(result == 0)
      }
      test("sample 4") {
        val freqs = List(7, 7, -2, -7, -4)
        val result = Day01.detectDuplicateFreq(freqs)
        assert(result == 14)
      }
    }
  }
}
