package Day24

import utils._

type Pos = (Int, Int)

extension (p: Pos)
  def +(p2: Pos): Pos = (p._1 + p2._1, p._2 + p2._2)

case class Area (val points: Set[Pos], val h: Int, val w: Int)

val neibor = Set((-1, 0), (1, 0), (0, -1), (0, 1))

def parseState(lines: List[String]): Area = 
  val points = (for {
    y <- 0 until lines.size
    x <- 0 until lines(0).size
    if lines(y)(x) == '#'
  } yield ((x, y)))
  .toSet

  Area(points, lines.size, lines(0).size)

def getNextCell(a: Area, p: Pos): Boolean = 
  val nc = neibor
    .map(p + _)
    .count(a.points.contains(_))

  //println(s"at $p has $nc active")

  if a.points.contains(p)
  then nc == 1
  else nc == 1 || nc == 2

def nextState(a: Area): Area =
  val points = (for {
    y <- 0 until a.h
    x <- 0 until a.w
    if getNextCell(a, (x, y))
  } yield ((x, y))).toSet

  a.copy(points = points)

case class SimState(val a: Area, val pses: Set[Set[Pos]] = Set())

def findSameState(s: SimState): Set[Pos] =
  if s.pses.contains(s.a.points)
  then return s.a.points
  else findSameState(SimState(nextState(s.a), s.pses + s.a.points))

def part1(lines: List[String]): Int = 
  val state = parseState(lines)

  val ps = findSameState(SimState(state))

  ps.map(p => p._2 * state.w + p._1).map(i => 1 << i).sum

type P3 = (Int, Int, Int)
case class A3(val points: Set[P3], val h: Int, val w: Int)

def getNeibors(p: P3): Set[P3] =
  neibor
    .flatMap(n => (n._1 + p._1, n._2 + p._2, p._3) match {
      case (-1, _, _) => List((1, 2, p._3 - 1))
      case (5, _, _) => List((3, 2, p._3 - 1))
      case (_, -1, _) => List((2, 1, p._3 - 1))
      case (_, 5, _) => List((2, 3, p._3 - 1))
      case (2, 2, _) => n match {
        case (-1, 0) => List((4, 0, p._3 + 1),(4, 1, p._3 + 1),
          (4, 2, p._3 + 1),(4, 3, p._3 + 1),(4, 4, p._3 + 1))
        case (1, 0) => List((0, 0, p._3 + 1),(0, 1, p._3 + 1),
          (0, 2, p._3 + 1),(0, 3, p._3 + 1),(0, 4, p._3 + 1))
        case (0, -1) => List((0, 4, p._3 + 1),(1, 4, p._3 + 1),
          (2, 4, p._3 + 1),(3, 4, p._3 + 1),(4, 4, p._3 + 1))
        case (0, 1) => List((0, 0, p._3 + 1),(1, 0, p._3 + 1),
          (2, 0, p._3 + 1),(3, 0, p._3 + 1),(4, 0, p._3 + 1))
      }
      case sp => List(sp)
    })

def getNextCell3(a: A3, p: P3): Boolean = 
  val nc = getNeibors(p)
    .count(a.points.contains(_))

  if a.points.contains(p)
  then nc == 1
  else nc == 1 || nc == 2

def nextState3(a: A3): A3 =
  val levelsToProcess = a.points
    .flatMap(p => List(p._3, p._3 - 1, p._3 + 1))
    .toSet

  val points = (for {
    y <- 0 until a.h
    x <- 0 until a.w
    z <- levelsToProcess
    if x != 2 || y != 2
    if getNextCell3(a, (x, y, z))
  } yield ((x, y, z))).toSet

  a.copy(points = points)

def nextState3(a: A3, n: Int): A3 =
  if n == 0
  then return a
  else nextState3(nextState3(a), n - 1)

def part2(lines: List[String], n: Int = 200): Int =
  val state = parseState(lines)
  val s3 = A3(state.points.map(p => (p._1, p._2, 0)), state.h, state.w)

  val ps = nextState3(s3, n)

  //ps.points.groupBy(_._3)
    //.view
    //.mapValues(vs => vs.map(p => (p._1, p._2)))
    //.toList
    //.sortBy((k, v) => k)
    //.foreach((k, v) => {
      //println(s"level $k")
      //printArea(v)
    //})

  ps.points.size
