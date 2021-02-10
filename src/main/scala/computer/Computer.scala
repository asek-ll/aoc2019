package computer

object Computer:

  def run(program: IntCode, input: IntCode = List()): State =
    run(State(program, input=input))

  def run(state: State): State = 
    val nextState = step(state)
    nextState match {
      case None => state
      case Some(st) => run(st)
    }

  def step(state: State): Option[State] = 
    state.status match {
      case Status.Ok => {
        val opCode = state.program(state.pos)
        val nextState = Instruction(opCode) match {
          case Some(instruction) => instruction(state)
          case _ => state.copy(status=Status.Error(s"Invalid instruction with code $opCode"))
        }
        Some(nextState)
      }
      case _ => None
    }
