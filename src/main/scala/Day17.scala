package Day17

import computer._

type Pos = (Int, Int)

enum Command:
  override def toString: String = this match {
    case TurnLeft => "L"
    case TurnRight => "R"
    case MoveForfard(x) => x.toString
  }
  case TurnLeft
  case TurnRight
  case MoveForfard(val units: Int)

enum Direction(val diff: Pos):
  def turnLeft = this match {
    case Up => Left
    case Left => Down
    case Down => Right
    case Right => Up
  }
  def turnRight = this match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }

  case Up extends Direction((0, -1))
  case Down extends Direction((0, 1))
  case Left extends Direction((-1, 0))
  case Right extends Direction((1, 0))

case class Robot(pos: Pos, dir: Direction)

extension (p: Pos)
  def +(p2: Pos): Pos = (p._1 + p2._1, p._2 + p2._2)

extension (p: Pos)
  def -(p2: Pos): Pos = (p._1 - p2._1, p._2 - p2._2)

extension (p: Pos)
  def len: Int = math.abs(p._1) + math.abs(p._2)

val neibors = Set((-1,0),(1,0),(0,-1),(0,1))


def getScaffoldings(view: Array[String]): Set[Pos] =
  (for {
    y <- 0 until view.size
    x <- 0 until view(0).size
    if view(y)(x) == '#'
  } yield ((x, y)))
    .toSet

def getAligmentParameter(view: Array[String]): Int =
  val scaffoldings = getScaffoldings(view)

  scaffoldings
    .filter(p => neibors.forall(n => scaffoldings.contains(n + p)))
    .map(p => p._1 * p._2)
    .sum


def part1(codes: IntCode): Int =
  
  val view = Computer.run(codes).output
    .map(_.toInt.toChar)
    .mkString
    .split("\n")


  getAligmentParameter(view)

def moveForward(scaffoldings: Set[Pos], robot: Robot): Robot =
  val nextPos = robot.pos + robot.dir.diff
  scaffoldings.contains(nextPos) match {
    case true => moveForward(scaffoldings, robot.copy(pos = nextPos))
    case _ => robot
  }

def findPath(scaffoldings: Set[Pos], robot: Robot): List[Command] =
  val turn = List(
    (List(Command.TurnLeft), robot.dir.turnLeft),
    (List(Command.TurnRight), robot.dir.turnRight),
    (List(), robot.dir),
  )
    .filter(p => scaffoldings.contains(p._2.diff + robot.pos))

  turn match {
    case commandAndDir :: _ => {
      val turned = moveForward(scaffoldings, robot.copy(dir = commandAndDir._2))
      val units = (turned.pos - robot.pos).len
      (commandAndDir._1 :+ Command.MoveForfard(units)) ::: findPath(scaffoldings, turned) 
    }
    case _ => List()
  }

def detectRobot(view: Array[String]): Robot =
  val roboPos = (for {
    y <- 0 until view.size
    x <- 0 until view(0).size
    if view(y)(x) != '#' && view(y)(x) != '.'
  } yield ((x, y)))(0)

  val roboDir = view(roboPos._2)(roboPos._1) match {
    case '^' => Direction.Up
    case '<' => Direction.Left
    case '>' => Direction.Right
    case 'V' => Direction.Down
  }
  
  Robot(roboPos, roboDir)

case class SplitState(val mainRoute: List[Int], val funcs: List[List[Command]]):
  def toFunctions: List[String] =
    val main = mainRoute
      .map(idx => (idx + 65).toChar)
      .mkString("",",","\n")

    val defs = funcs
      .map(cms => cms.map(_.toString).mkString("",",","\n"))

    main :: defs


def defineNewFunction(cmds: List[Command], state: SplitState): Option[SplitState] =
  val init: Option[SplitState] = None
  (1 until math.min(cmds.size, 11))
    .foldLeft(init)((res, newFuncSize) => {
      res match {
        case None => {
          val newFunc = cmds.slice(0, newFuncSize)
          splitByFucntions(cmds.slice(newFuncSize, cmds.size), state.copy(mainRoute = state.mainRoute :+ (state.funcs.size),funcs = state.funcs :+ newFunc))
        }
        case x => x
      }
    })

def applyFunc(cmds: List[Command], state: SplitState): Option[SplitState] =
  val init: Option[SplitState] = None
  state.funcs.indices
    .filter(fidx => {
      val f = state.funcs(fidx)
      f.size <= cmds.size && cmds.slice(0, f.size) == f
    })
    .foldLeft(init)((res, fidx) => {
      res match {
        case None => splitByFucntions(
          cmds.slice(state.funcs(fidx).size, cmds.size),
          state.copy(mainRoute = state.mainRoute :+ fidx)
        )
        case x => x
      }
    })

def splitByFucntions(cmds: List[Command], state: SplitState): Option[SplitState] =
  cmds match {
    case List() => Some(state)
    case _ => state.funcs match {
      case Nil => defineNewFunction(cmds, state)
      case x if x.size == 3 => applyFunc(cmds, state)
      case _ => defineNewFunction(cmds, state).orElse(applyFunc(cmds, state))
    }
  }

def getRouteProgram(view: Array[String]): SplitState =
  val scaffoldings = getScaffoldings(view)

  val robot = detectRobot(view)

  val path = findPath(scaffoldings, robot)

  splitByFucntions(path, SplitState(List(), List())).get


def part2(codes: IntCode): Int =

  val view = Computer.run(codes).output
    .map(_.toInt.toChar)
    .mkString
    .split("\n")

  val res = getRouteProgram(view)

  val input = (res.toFunctions :+ "n\n")
    .mkString
    .map(_.toInt)
    .toList

  val compState = Computer.run(codes.updated(0, 2), input).output

  compState.last.toInt
