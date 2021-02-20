package Day20

import scala.collection.mutable.{PriorityQueue, ArrayBuffer, HashSet}

type Pos = (Int, Int)
extension (p: Pos)
  def +(p2: Pos): Pos = (p._1 + p2._1, p._2 + p2._2)

extension (p: Pos)
  def -(p2: Pos): Pos = (p._1 - p2._1, p._2 - p2._2)

type Area = List[String]
extension (a: Area)
  def atPos(p: Pos) = a(p._2)(p._1)

val neibors = Set((1, 0), (-1, 0), (0, 1), (0, -1))

case class Portal(val label: String, val in: Pos, val out: Pos, val isOuter: Boolean)

def isOutPortal(a: Area, out: Pos): Boolean =
  out._1 == 2 || out._2 == 2 || (a(0).size - out._1 - 1) == 2 || (a.size - out._2 - 1) == 2

def parsePortal(area: Area, sp: Pos): Option[Portal] =
  Seq((0, 1), (1, 0))
    .filter(d => {
      val p = d + sp
      p._2 < area.size && p._1 < area(p._2).size && area.atPos(p).isLetter
    })
    .map(d => {
      val p = d + sp
      val label = Seq(area.atPos(sp), area.atPos(p)).mkString

      val inOut = Seq((p, p + d), (sp, sp - d))
        .filter((in, out) => 
            out._2 >= 0 &&
            out._1 >= 0 &&
            out._2 < area.size &&
            out._1 < area(out._2).size &&
            area.atPos(out) == '.')
        .head

      Portal(label, inOut._1, inOut._2, isOutPortal(area, inOut._2))
    })
    .headOption


case class Linking(val portalMap: Map[Pos, Portal], val start: Pos, val end: Pos)
def getPortals(area: Area): Linking =

  val portals = (for {
    y <- 0 until area.size
    x <- 0 until area(0).size
    if area(y)(x).isLetter 
  } yield ((x, y)))
  .map(parsePortal(area, _))
  .collect({
    case Some(p) => p
  })


  val portalsByLabel = portals
    .groupBy(_.label)


  val portalMap = portalsByLabel
    .filter((l, ps) => l != "AA" && l != "ZZ")
    .flatMap((l, ps) => Seq(ps(0).in -> ps(1), ps(1).in -> ps(0)))
    .toMap

  Linking(portalMap, portalsByLabel("AA")(0).out, portalsByLabel("ZZ")(0).out)


def explore(area: Area, linking: Linking): Int = 
  val queue = PriorityQueue((linking.start, 0))(Ordering.by[(Pos, Int), Int](_._2).reverse)
  val visited: HashSet[Pos] = HashSet()

  while (!queue.isEmpty) {
    val (pos, dist) = queue.dequeue()  

    if pos == linking.end then return dist

    if !visited.contains(pos) then {
      visited += pos

      val ch = area(pos._2)(pos._1)

      neibors
        .map(_ + pos)
        .map(p => linking.portalMap.get(p) match {
          case Some(d) => d.out
          case _ => p
        })
        .filter(p => 
            p._1 >= 0 &&
            p._2 >= 0 && p._2 < area.size &&
            p._1 < area(p._2).size &&
            area(p._2)(p._1) == '.' &&
            !visited.contains(p)
            )
            .foreach(p => queue.enqueue((p, dist+1)))
    }
  }
  -1

def exploreRecursive(area: Area, linking: Linking): Int = 
  val queue = PriorityQueue((linking.start, 0, 0))(Ordering.by[(Pos, Int, Int), Int](_._2).reverse)
  val visited: HashSet[(Pos, Int)] = HashSet()

  while (!queue.isEmpty) {
    val (pos, level, dist) = queue.dequeue()  
    //println((pos, level, dist))

    if pos == linking.end && level == 0 then return dist
    //if dist > 26 then return -1;

    val pos3 = (pos, level)
    if !visited.contains((pos3)) then {
      visited += pos3

      val ch = area(pos._2)(pos._1)

      //println(area.updated(pos._2, area(pos._2).updated(pos._1, 'X')).mkString("\n"))

      neibors
        .map(_ + pos)
        .map(p => linking.portalMap.get(p) match {
          case Some(d) => if d.isOuter then (d.out, level+1) else (d.out, level-1)
          case _ => (p, level)
        })
        .filter((p, d) => 
            p._1 >= 0 &&
            p._2 >= 0 && p._2 < area.size &&
            p._1 < area(p._2).size &&
            area(p._2)(p._1) == '.' &&
            !visited.contains((p, d)) &&
            d >= 0
            )
            .foreach(p => queue.enqueue((p._1, p._2, dist+1)))
    }
  }
  -1


def part1(area: Area): Int = explore(area, getPortals(area))

def part2(area: Area): Int =
  val ps = getPortals(area)
  exploreRecursive(area, ps)

