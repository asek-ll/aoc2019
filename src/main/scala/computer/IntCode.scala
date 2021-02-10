package computer

type Code = Int | Long

type IntCode = List[Code]

extension (c: Code)
  def toInt: Int = c match {
    case i: Int => i
    case l: Long => l.toInt
  }

extension (c: Code)
  def toLong: Long = c match {
    case i: Int => i.toLong
    case l: Long => l
  }

extension (c1: Code)
  def +(c2: Code): Code = {
    val sum = c1.toLong + c2.toLong
    if sum.isValidInt
    then sum.toInt
    else sum
  }

extension (c1: Code)
  def *(c2: Code): Code = {
    val sum = c1.toLong * c2.toLong
    if sum.isValidInt
    then sum.toInt
    else sum
  }

extension (c1: Code)
  def <(c2: Code): Boolean = c1.toLong < c2.toLong

extension (lines: List[String])
  def toIntCode: IntCode =
    lines(0)
      .split(",")
      .map(_.toInt)
      .toList
  
