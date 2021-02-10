package computer

enum Status:
  case Ok
  case Halted
  case InputWaited
  case Error(msg: String)

case class State(
  val program: IntCode,
  val pos: Int = 0,
  val status: Status = Status.Ok,
  val input: IntCode = List(),
  val output: IntCode = List(),
  val relativeBase: Int = 0,
  )
