package computer.AsciiComputer

import computer._


def run(codes: IntCode, input: List[String]): Either[String, IntCode] =
  val inputCodes = input.map(_ :+ '\n')
    .mkString
    .map(_.toInt)
    .toList


  val out = Computer.run(codes, inputCodes).output

  val isAscii = out.forall(_ < 0xFF)

  if isAscii 
  then Left(out.map(_.toInt.toChar).mkString)
  else Right(out)
