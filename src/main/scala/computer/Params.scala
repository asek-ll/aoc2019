package computer

enum Mode(val id: Int):
  case Position extends Mode(0)
  case Immediate extends Mode(1)
  case Relative extends Mode(2)

object Mode:
  private val modeById = Mode.values
    .map(m => m.id -> m)
    .toMap

  def apply(id: Int): Mode = modeById(id)


case class Params(modes: Int, val program: IntCode, val basePos: Int, val relativeBase: Int):

  private val mods = modes.toString
    .reverse
    .map(ch => Mode(ch.asDigit))
    .toList

  private def getModeForPos(pos: Int): Mode =
    pos match {
      case x if x <= mods.size => mods(x-1)
      case _ => Mode.Position
    }

  def apply(pos: Int): Code = 
    val mode = getModeForPos(pos)
    val address = mode match {
      case Mode.Position => program(basePos + pos).toInt
      case Mode.Immediate => basePos + pos
      case Mode.Relative => program(basePos + pos).toInt + relativeBase
    }
    if address >= program.size
    then 0
    else program(address)

  def updated(pos: Int, value: Code): IntCode =
    val address = getModeForPos(pos) match {
      case Mode.Position => program(basePos + pos).toInt
      case Mode.Relative => program(basePos + pos).toInt + relativeBase
      case Mode.Immediate => throw new IllegalArgumentException("Unsopported immediate mode for output param")
    }

    (address - program.size) match {
      case x if x < 0 => program.updated(address, value)
      case 0 => program :+ value
      case x => program ::: (List.fill(x)(0) :+ value)
    }
