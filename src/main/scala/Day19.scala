package Day19

import computer._

type Pos = (Int, Int)

def check(code: IntCode, x: Int, y: Int): Int =
  Computer.run(code, List(x, y)).output.head.toInt

def part1(code: IntCode): Int =


  //println((0 until 50)
    //.map(y => (0 until 50).map(x => check(code, x, y) match {
      //case 1 => '#'
      //case _ => '.'
    //}).mkString)
    //.mkString("\n"))

  val res = for {
    x <- 0 until 50
    y <- 0 until 50
  } yield check(code, x, y)

  res.sum


case class ScanState(val level: Int, val lowerBound: List[Pos], val upperBound: List[Pos])

def searchXFor(level: Int, startX: Int, target: Int, code: IntCode): Int  = 
  check(code, startX, level) match {
    case x if x == target => startX
    case _ => searchXFor(level, startX + 1, target , code)
  }

def nextScan(state: ScanState, code: IntCode): ScanState = 
    val minX = state.lowerBound.last._1
    val maxX = state.upperBound.last._1

    val level = state.level + 1
    val nextMinX = searchXFor(level, minX, 1, code)
    val nextMaxX = searchXFor(level, math.max(maxX, nextMinX + 1), 0, code) - 1

    ScanState(level,
      state.lowerBound :+ (nextMinX, level),
      state.upperBound :+ (nextMaxX, level)
      )

def search(state: ScanState, code: IntCode, bound: Pos): Pos =
    val lower = state.lowerBound.last
    val hypo = (lower._1 + bound._1-1, lower._2 - bound._2+1)
    hypo._2 match {
      case y if y >= 0 && state.upperBound(y)._1 >= hypo._1 => (lower._1, y)
      case _ => search(nextScan(state, code), code, bound)
    }


def part2(code: IntCode): Int = 

  val startBound = List((0, 0),(-1, -1), (-1, -1), (2, 3), (-1, -1), (3, 5))
  val s = ScanState(startBound.size-1, startBound, startBound)
  search(s, code, (100, 100))
    .toList
    .foldLeft(0)((a, c) => a * 10000 + c)
