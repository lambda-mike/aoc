package day03

import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = readInput("day03/src/input.txt")
    // 101781
    println("Solving Day03A...")
    val resultA = solveA(input)
    println(resultA)
    // 909
    println("Solving Day03B...")
    val resultB = solveB(input)
    println(resultB)
  }

  def readInput(file: String): List[String] =
    Source.fromFile(file).getLines.toList

  final case class Pos(x: Int, y: Int)

  // #3 @ 5,5: 2x2
  final case class Claim(id: Int, pos: Pos, width: Int, height: Int)

  def parseClaim(input: String): Claim = {
    val words = input.tail.split(" ").toList
    words match {
      case id :: _at :: posStr :: dimStr =>
        val (xStr :: yStr) = posStr.init.split(",").toList
        val x = xStr.toInt
        val y = yStr.head.toInt
        val (wStr :: hStr) = dimStr.head.split("x").toList
        val w = wStr.toInt
        val h = hStr.head.toInt
        Claim(id.toInt, Pos(x, y), w, h)
    }
  }

  // Split claim into 1x1 pieces and merge them all together into List
  def explodeClaim(claim: Claim): List[Claim] = {
    for {
      x <- (0 until claim.width).toList
      y <- (0 until claim.height).toList
      val pos = Pos(claim.pos.x + x, claim.pos.y + y)
    } yield Claim(claim.id, pos, 1, 1)
  }

  def groupClaimsPerInch(claims: List[Claim]): Map[Pos, Set[Int]] = {
    def mergeClaim(
        claims: Map[Pos, Set[Int]],
        claim: Claim
    ): Map[Pos, Set[Int]] =
      claims.updatedWith(claim.pos)(_ match {
        case Some(ids) => Some(ids + claim.id)
        case None      => Some(Set(claim.id))
      })
    claims.flatMap(explodeClaim).foldLeft(Map[Pos, Set[Int]]())(mergeClaim)
  }

  def solveA(lines: List[String]): Int =
    groupClaimsPerInch(lines.map(parseClaim)).count { case (_key, ids) =>
      ids.size >= 2
    }

  def solveB(lines: List[String]): Int = {
    val claimsPerInch = groupClaimsPerInch(lines.map(parseClaim))
    val (potentiallyNonOverlapping, overlapping) = claimsPerInch.partition {
      case (_, ids) =>
        ids.size == 1
    }
    val potentiallyNonOverlappingIds =
      potentiallyNonOverlapping.foldLeft(Set[Int]()) { case (all, (_, ids)) =>
        all ++ ids
      }
    val overlappingIds =
      overlapping.foldLeft(Set[Int]()) { case (all, (_, ids)) => all ++ ids }
    val nonOverlappingIds = potentiallyNonOverlappingIds.diff(overlappingIds)
    val nonOverlappingIdsSize = nonOverlappingIds.size
    assert(
      nonOverlappingIdsSize == 1,
      s"Non overlapping ids size should be 1, got: ${nonOverlappingIdsSize}"
    )
    nonOverlappingIds.head
  }
}
