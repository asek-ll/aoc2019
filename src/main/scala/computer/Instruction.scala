package computer


case class Instruction(val operation: Operation, val modes: Int):
  def apply(state: State): State =
    operation(Params(modes, state.program, state.pos, state.relativeBase), state)

trait Operation:
  def apply(params: Params, state: State): State

object Instruction:
  def apply(instructionCode: Code): Option[Instruction] =
    val intCode = instructionCode.toInt
    val opCode = intCode % 100
    val modes = intCode / 100

    val operation = opCode match {
      case 1 => Some(Add)
      case 2 => Some(Mul)
      case 3 => Some(Input)
      case 4 => Some(Output)
      case 5 => Some(JumpIfTrue)
      case 6 => Some(JumpIfFalse)
      case 7 => Some(LessThan)
      case 8 => Some(Equals)
      case 9 => Some(AdjustRelativeBase)
      case 99 => Some(Halt)
      case _ => None
    }

    operation
      .map(op => Instruction(op, modes))

object Add extends Operation:
  def apply(params: Params, state: State): State =
    val result = params(1) + params(2)

    val program = params.updated(3, result) 

    state.copy(program = program, pos = state.pos + 4)

object Mul extends Operation:
  def apply(params: Params, state: State): State =
    val result = params(1) * params(2)

    val program = params.updated(3, result) 

    state.copy(program = program, pos = state.pos + 4)

object Halt extends Operation:
  def apply(params: Params, state: State): State =
    state.copy(status = Status.Halted)

object Input extends Operation:
  def apply(params: Params, state: State): State =

    state.input match {
      case head :: tail => {
        val program = params.updated(1, head) 
        state.copy(program = program, pos = state.pos + 2, input = tail)
      }
      case _ => state.copy(status = Status.InputWaited)
    }

  
object Output extends Operation:
  def apply(params: Params, state: State): State =
    state.copy(pos = state.pos + 2, output = state.output :+ params(1))

object JumpIfTrue extends Operation:
  def apply(params: Params, state: State): State =
    val newPos = if params(1) != 0 
    then params(2).toInt
    else state.pos + 3

    state.copy(pos = newPos)

object JumpIfFalse extends Operation:
  def apply(params: Params, state: State): State =
    val newPos = if params(1) == 0 
    then params(2).toInt
    else state.pos + 3

    state.copy(pos = newPos)


object LessThan extends Operation:
  def apply(params: Params, state: State): State =
    val value = if params(1) < params(2) then 1 else 0

    state.copy(program = params.updated(3, value), pos = state.pos + 4)

object Equals extends Operation:
  def apply(params: Params, state: State): State =
    val value = if params(1) == params(2) then 1 else 0

    state.copy(program = params.updated(3, value), pos = state.pos + 4)

object AdjustRelativeBase extends Operation:
  def apply(params: Params, state: State): State =
    state.copy(pos = state.pos + 2, relativeBase = state.relativeBase + params(1).toInt)
