package Day25

import scala.collection.mutable.{PriorityQueue, ArrayBuffer, HashSet}
import computer._
import computer.AsciiComputer.getAsciiOutput

case class RoomInfo(val name: String, val doors: Set[String], val items: Set[String])

def splitByBlocks(s: List[String]): List[List[String]] =
  val trimmed = s.dropWhile(_.isEmpty)
  trimmed.indexOf("") match {
    case -1 => List(trimmed)
    case x => {
      val (head, tail) = trimmed.splitAt(x)
      head :: splitByBlocks(tail)
    }
  }

def parseOutput(lines: List[String]): RoomInfo = 
  val blocks = splitByBlocks(lines)
    .map(b => (b(0).takeWhile(_ != ' ')) -> b)
    .toMap

  val roomName = blocks("==")(0).slice(3, blocks("==")(0).size - 3)

  val dirs = blocks.get("Doors")
    .map(ls => ls.tail.map(_.stripPrefix("- ")).toSet)
    .getOrElse(Set())

  val items = blocks.get("Items")
    .map(ls => ls.tail.map(_.stripPrefix("- ")).filter(i => !blacklistItems.contains(i)).toSet)
    .getOrElse(Set())

  RoomInfo(roomName, dirs, items)

val blacklistItems = Set(
  "infinite loop",
  "photons",
  "escape pod",
  "giant electromagnet",
  "molten lava"
  )

case class ExploreState(comp: State, val dist: Int, steps: List[String] = List())

def explore(initState: ExploreState, codes: IntCode): ArrayBuffer[(RoomInfo, List[String])] = 
  val queue = PriorityQueue(initState)(Ordering.by[ExploreState, Int](_.dist).reverse)
  val visited: HashSet[String] = HashSet()

  val roomsAndSteps: ArrayBuffer[(RoomInfo, List[String])] = ArrayBuffer()

  while (!queue.isEmpty) {
    val ExploreState(comp, dist, steps) = queue.dequeue()  
    val currentRoom = parseOutput(comp.getAsciiOutput)
    val roomName = currentRoom.name

    if !visited.contains(roomName) then {
      visited += roomName

      roomsAndSteps += ((currentRoom, steps))

      currentRoom.doors
        .foreach(d => queue.enqueue(ExploreState(AsciiComputer.run(comp, List(d)), dist + 1, steps :+ d)))
    }
  }

  roomsAndSteps


val reverseDir = Map(
  "north" -> "south",
  "south" -> "north",
  "west" -> "east",
  "east" -> "west",
  )

def reversePath(path: List[String]): List[String] =
  path.map(reverseDir(_)).reverse

val password = raw"\d+".r

def part1(codes: IntCode): Int = 

  val rooms = explore(ExploreState(Computer.run(codes), 0), codes)

  val allItems = rooms
    .flatMap((r, s) => r.items)
    .toSet

  val securityPath = rooms.find((r, s) => r.name == "Security Checkpoint")
    .map((r, s) => s)
    .get


  val itemsGatherProgram = rooms
    .filter((r, s) => r.items.nonEmpty)
    .flatMap((r, s) => 
        r.items.map(i => 
            (s :+ s"take $i") ::: reversePath(s))
          )
    .reduce(_ ::: _)

  val gatherItemsAndGoToSecurity = itemsGatherProgram ::: securityPath

  val securePoint = AsciiComputer.run(State(codes), gatherItemsAndGoToSecurity)

  allItems.subsets.map(ss => {
      val dropCommands = ss.map(i => s"drop $i").toList
      val res = AsciiComputer.run(securePoint, dropCommands :+ "north")
      
      val out = res.getAsciiOutput.mkString("\n")
      password.findFirstIn(out).map(_.toInt)
    })
    .find(_.isDefined)
    .get.get
