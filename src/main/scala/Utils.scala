package utils

import scala.io.Source

def fromPuzzleInput(day: Int): List[String] =
  Source.fromFile(s"inputs/day${day}.txt")
    .getLines
    .toList

def printArea(ps: Set[(Int, Int)], existSymbol: Char = '#', emptySymbol: Char = '.'): Unit =
  val minMax = ps
    .foldLeft((0, 0, 0, 0))((mm, p) => (math.min(mm._1, p._1), math.min(mm._2, p._2), math.max(mm._3, p._1), math.max(mm._4, p._2)))
    val view = (minMax._2 to minMax._4)
      .map(y => (minMax._1 to minMax._3).map(x => ps.contains((x, y)) match {
        case true => existSymbol
        case false => emptySymbol
      }).mkString)
      .mkString("\n")

    println(view)

def time[R](block: => R): R = {
    val t0 = System.nanoTime() / 1000000
    val result = block    // call-by-name
    val t1 = System.nanoTime() / 1000000
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
}
