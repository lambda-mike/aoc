package day01

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = readInput("day01/src/input.txt")
    // 510
    println("Solving Day01A...")
    val resultA = solveA(input)
    println(resultA)
    // 69074
    println("Solving Day01B...")
    val resultB = solveB(input)
    println(resultB)
  }

  def readInput(file: String): List[String] =
    Source.fromFile(file).getLines.toList

  def parseFreqChanges(lines: List[String]): List[Int] =
    lines.map(_.toInt)

  def sumFreqChanges(lines: List[Int]): Int =
    lines.sum

  def solveA(lines: List[String]): Int =
    sumFreqChanges(parseFreqChanges(lines))

  def detectDuplicateFreq(allChanges: List[Int]): Int = {
    def go(currentFreq: Int, changes: List[Int], seenFreqs: Set[Int]): Int = {
      if (seenFreqs.contains(currentFreq)) {
        currentFreq
      } else {
        val (change, remainingChanges) = changes match {
          case c :: cs => (c, cs)
          case Nil     => (allChanges.head, allChanges.tail)
        }
        val newFreq = currentFreq + change
        val newSeenFreqs = seenFreqs + currentFreq
        go(newFreq, remainingChanges, newSeenFreqs)
      }
    }
    go(0, allChanges, Set())
  }

  def solveB(lines: List[String]): Int =
    detectDuplicateFreq(parseFreqChanges(lines))

}
