package Day18

import scala.collection.mutable.{PriorityQueue, ArrayBuffer, HashSet}

type Pos = (Int, Int)
extension (p: Pos)
  def +(p2: Pos): Pos = (p._1 + p2._1, p._2 + p2._2)

extension (p: Pos)
  def -(p2: Pos): Pos = (p._1 - p2._1, p._2 - p2._2)

type Area = List[String]

def findPoints(map: Area): Seq[(Point, Pos)] =
   (0 until map.size)
     .flatMap(y => (0 until map(0).size).map(x => parsePoint(map(y)(x)).map(p => (p -> (x,y)))))
     .filter(_.isDefined)
     .map(_.get)

val neibors = Set((1, 0), (-1, 0), (0, 1), (0, -1))

enum Point:
  override def toString: String = this match {
    case Start => "Start"
    case Key(id) => s"Key ${(id + 'a'.toInt).toChar}"
    case Door(id) => s"Door ${(id + 'A'.toInt).toChar}"
  }

  case Start
  case Key(val id: Int)
  case Door(val id: Int)

type Keyring = Set[Int]


case class Target(val pos: Pos, val dist: Int, val point: Point):
  def isDoor: Boolean = point match {
    case Point.Door(_) => true
    case _ => false
  }

def parsePoint(ch: Char): Option[Point] = ch match {
  case '@' => Some(Point.Start)
  case x if x.isUpper => Some(Point.Door(x.toInt - 'A'.toInt))
  case x if x.isLower => Some(Point.Key(x.toInt - 'a'.toInt))
  case _ => None
}
  

def explore(area: Area, initPos: Pos): List[Target] = 
  val queue = PriorityQueue((initPos, 0))(Ordering.by[(Pos, Int), Int](_._2).reverse)
  val visited: HashSet[Pos] = HashSet()
  val targets: ArrayBuffer[Target] = ArrayBuffer()


  while (!queue.isEmpty) {
    val (pos, dist) = queue.dequeue()  
    if !visited.contains(pos) then {
      visited += pos

      val ch = area(pos._2)(pos._1)
      val target = if initPos == pos 
      then None
      else parsePoint(ch).map(p => Target(pos, dist, p))

      target.foreach(t => targets += t)

      val doProcess = target match {
        case Some(t) if t.isDoor => false 
        case _ => true
      }

      if doProcess
      then {

        neibors
          .map(_ + pos)
          .filter(p => 
              p._1 >= 0 &&
              p._2 >= 0 && p._2 < area.size &&
              p._1 < area(p._2).size &&
              area(p._2)(p._1) != '#' &&
              !visited.contains(p)
              )
                .foreach(p => queue.enqueue((p, dist+1)))
      }
    }
  }
  targets.toList


type Relations = Map[Point, Map[Point, Int]]
def exp(area: Area): Relations = 

  val points = findPoints(area)
  points
    .flatMap((point, pos) => {
      val ts = explore(area, pos)
      ts.map(t => (point, t.point, t.dist))
    })
    .groupBy(_._1)
    .view.mapValues(ts => ts.map(t => (t._2 -> t._3)).toMap)
    .toMap



case class StateKey(val point: Point, val keys: Keyring = Set())
case class State(val key: StateKey, val dist: Int = 0)

def search(rels: Relations, initState: State): Int =
  val queue = PriorityQueue(initState)(Ordering.by[State, Int](_.dist).reverse)
  val visited: HashSet[StateKey] = HashSet()
  
  while (!queue.isEmpty) {
    val from = queue.dequeue()  
    if !visited.contains(from.key) then {
      visited += from.key

      val targets = getNext(SearchState(from.key.point, from.key.keys), rels)

      if targets.isEmpty then return from.dist
      for (t <- targets) {
        val dist = from.dist + t._2
        val keys = from.key.keys + t._1.id
        val nextState = State(StateKey(t._1, keys), dist)
        queue.enqueue(nextState)
      }
    }
  }

  return -1
  

case class SearchState(val current: Point, val keys: Keyring = Set())

def getNext(state: SearchState, rels: Relations, visited: Set[Point] = Set()): List[(Point.Key, Int)] =
  val relPoints = rels(state.current)
    .filter((k, v) => !visited.contains(k))

  val keysRel: Map[Point.Key, Int] = relPoints.collect({
    case (p: Point.Key, dist) if !state.keys.contains(p.id) => (p -> dist)
  })

  val nextVisited = visited ++ relPoints.keySet + state.current

  val nextToOpenDoors: Map[Point.Door, Int]  = relPoints.collect{
    case (p: Point.Door, dist) if state.keys.contains(p.id) => (p -> dist)
  }
  
  val result = nextToOpenDoors
    .flatMap(opd => 
        getNext(state.copy(current = opd._1), rels, nextVisited)
          .map(pd => (pd._1, pd._2 + opd._2)))
    .foldLeft(keysRel)((m, pd) => {
      m.get(pd._1) match {
        case Some(d) if d < pd._2 => m + pd 
        case _ => m + pd
      }
    })

  result
    .toList
    .sortBy(_._2)


def part1(area: Area): Int =
  val rels = exp(area)
  search(rels, State(StateKey(Point.Start)))
