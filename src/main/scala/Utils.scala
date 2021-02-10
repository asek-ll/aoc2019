package utils

import scala.io.Source

def fromPuzzleInput(day: Int): List[String] =
  Source.fromFile(s"inputs/day${day}.txt")
    .getLines
    .toList


def time[R](block: => R): R = {
    val t0 = System.nanoTime() / 1000000
    val result = block    // call-by-name
    val t1 = System.nanoTime() / 1000000
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
}
