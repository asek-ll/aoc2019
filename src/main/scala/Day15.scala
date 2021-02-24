package Day15

import computer._
import scala.language.implicitConversions
import utils._

val smalMap =
"""_##___
#..##_
#.#..#
#.O.#_
_###__"""

val cachedMap = 
"""_#####_#############_###_###########_###_
#.....#.............#...#...........#...#
#.#####.#####.#######.#.#.#####.###.#.##_
#.#...#.....#.........#.......#.#.#.#...#
#.#.#.#####.###############.###.#.#.###.#
#...#.........#.#.......#...#...#.#.....#
_####.#######.#.#.#####.#.###.###.#####.#
#...#.#.....#.#.#.....#.#.#.#.#...#.....#
#.#.###.###.#.#.#####.#.#.#.#.#.#.#.####_
#.#.....#.#.#.#.....#.#.#...#.#.#.......#
#.#######.#.#.###.#.#.#.#####.#########.#
#...#...#.#.#.....#.#.#.....#.#.......#.#
#.#.#.#.#.#.###.#####.#####.#.#.#####.#.#
#.#...#.#...#...#.....#.......#.#...#.#.#
_######.#.#######.#####.#######.#.#.#.#.#
#...#...#.#.......#.....#...#...#.#.#.#.#
#.#.#.###.###.###.#######.#.#.###.###.#.#
#.#...#...#...#...#...#...#...#.#.....#.#
#.#####.###.#.#####.#.#.##_####.#.######_
#.....#...#.#.#...#.#...#.#.....#.#.....#
#.###.###.#.###.#.#.#####.#.###.#.#.###.#
#...#.#...#.....#.#.#...#.#.#.#.#...#.#.#
_##.#.#.#########.#.###.#.#.#.#.#####.#.#
#...#.#.........#.#.....#...#.#...#.....#
#.###.#########.#.#######.###.###.#.####_
#.#.#.#.............#.....#.....#.#.....#
#.#.#.#######.#######.#######.#.#.#####.#
#...#.......#.#.....#...#.....#.#.....#.#
_##.#######.###.###.###.#.#.#####.#####.#
#.#.......#.#...#.#.#...#.#.....#.......#
#.#######.#.#.###.#.#.#######.#.########_
#.........#.#.....#...#.....#.#...#.....#
#.#########.#####.#####.###.#.#.###.###.#
#.#.........#...#.......#.#.#.#.....#O#.#
_##.#######.#.#.#########.#.#.#######.#.#
#...#...#...#.#.........#...#.........#.#
#.###.###.###.#.#########.#####.#######.#
#.#...........#.#...#...#.#.....#.......#
#.###############.#.#.#.#.#######.#####.#
#.................#...#...........#.....#
_#################_###_###########_#####_"""


type Pos = (Int, Int)
extension (p1: Pos)
  def +(p2: Pos): Pos = (p1._1 + p2._1, p1._2 + p2._2)

extension (p1: Pos)
  def -(p2: Pos): Pos = (p1._1 - p2._1, p1._2 - p2._2)

def getPointCode(code: IntCode, p: Pos, m: Int): Int =

  val droidPos: Pos = (1034, 1035)

  if p == droidPos then return -1

  val diff = m match {
    case 1 => (0, 1)
    case 2 => (0, -1)
    case 3 => (1, 0)
    case 4 => (-1, 0)
  }




  val patched = code
    .updated(1034, p._1 + diff._1)
    .updated(1035, p._2 + diff._2)
    .updated(1039, p._1 + diff._1)
    .updated(1040, p._2 + diff._2)

  Computer.run(patched, List(m)).output.head.toInt

type Field = Map[Pos, Int]

case class State(field: Field, robot: Pos, steps: List[Int], isFinish: Boolean = false)


def turnRight(dir: Int): Int = dir match {
  case 1 => 4
  case 2 => 3
  case 3 => 1
  case 4 => 2
}

def move(code:IntCode, st: State, dir: Int): State =

  val dif = dir match {
    case 1 => (0, -1)
    case 2 => (0, 1)
    case 3 => (-1, 0)
    case 4 => (1, 0)
  }

  val targetPos = (st.robot._1 + dif._1, st.robot._2 + dif._2)


  val newSteps = st.steps :+ dir
  val res = Computer.run(code, newSteps).output.last

  val newField = st.field + (targetPos -> res.toInt)

  res match {
    case 0 => st.copy(field = newField)
    case _ => State(newField, targetPos, newSteps) 
  }
 

val neibors: Set[Pos] = Set((0, 1),(0, -1),(1, 0),(-1, 0))

def getNeibors(p: Pos): Set[Pos] =
  neibors
    .map(n => p + n)    
    .toSet


def getDirByNeibor(p: Pos): Int =
  p match {
    case (0, -1) => 1
    case (0, 1) => 2
    case (-1, 0) => 3
    case (1, 0) => 4
  }

def exploreNear(code: IntCode, st: State, neibors: Set[Pos]): (List[State], Field) =

  val nsts = neibors.map(nei => (nei, move(code, st, getDirByNeibor(nei - st.robot))))

  val newField = nsts.foldLeft(st.field)((fs, neiState) => 
      fs + (neiState._1 -> neiState._2.field(neiState._1))
  )

  val states = nsts
    .map(_._2)
    .filter(_.robot != st.robot)
    .toList

  (states, newField)


def backToPos(code: IntCode, st: State, p: Pos): State =
  if st.robot == p
  then st
  else {
    val stm = move(code, st, turnRight(turnRight(st.steps.last)))
    backToPos(code, stm.copy(steps=stm.steps.dropRight(2)), p)
  }


def exploreOxygen(code: IntCode, st: State): State =
  exploreUntil(code, st, (st) => st.field(st.robot) == 2)

def exploreAll(code: IntCode, st: State): State =
  exploreUntil(code, st, (st) => false)

def exploreUntil(code: IntCode, st: State, isFinish: (State) => Boolean): State =

  val unkownNeibors = getNeibors(st.robot)
    .filter(n => st.field.get(n) == None)

  val waysAndfield = exploreNear(code, st, unkownNeibors)

  waysAndfield._1
    .foldLeft(st.copy(field=waysAndfield._2))((state, neiState) => {
      isFinish(state) match {
        case true => state
        case _ => {
          val newField = state.field ++ neiState.field
          val neiWithActualField = neiState.copy(field=newField)


          exploreUntil(code, neiWithActualField, isFinish)
        }
      }
    })



def part1(code: IntCode): Int =

  val initState = State(Map((0,0) -> 1), (0, 0), List())

  val st = exploreOxygen(code, initState)

  st.steps.size

def fromMap(s: String): Field =
  val lines = s.split("\n")
  lines.indices
    .flatMap(y => {
      val line = lines(y)
      line.indices
        .filter(x => line(x) != '_')
        .map(x => {
          (x, y) -> (line(x) match {
            case '#' =>  0
            case 'O' =>  2
            case '.' =>  1
          })
        })
    })
    .toMap
  

def spreadO2(field: Field, toSpread: Set[Pos], time: Int = -1): Int =
  toSpread.toList match {
    case List() => time
    case xs => {
      val updated = field -- xs
      val nextToSpread = xs.flatMap(getNeibors(_))
        .filter(updated.contains(_))
        .toSet
      
      spreadO2(updated, nextToSpread, time + 1)
    }
  }

def part2(code: IntCode): Int =

  val initState = State(Map((0,0) -> 1), (0, 0), List())

  val st = exploreAll(code, initState)
  val field = st.field

  val clearField = field
    .filter((k, v) => v == 1 || v == 2)
    .toMap

  val o2 = clearField
    .find((k, v) => v == 2)
    .map(_._1)
    .get

  spreadO2(clearField, Set(o2))
