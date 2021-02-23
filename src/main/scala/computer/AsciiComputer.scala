package computer.AsciiComputer

import computer._
import scala.io.StdIn.readLine


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


def run(state: State, input: List[String] = List()): State =
  val inputCodes = input.map(_ :+ '\n')
    .mkString
    .map(_.toInt)
    .toList

  Computer.run(state.copy(status=Status.Ok, input=inputCodes, output=List()))

extension(s: State)
  def getAsciiOutput: List[String] =
    s.output.map(_.toInt.toChar).mkString.split("\n").toList

def interactive(codes: IntCode): Unit =

  var comp = Computer.run(codes)
  println(comp.output.map(_.toInt.toChar).mkString)

  while (comp.status == Status.InputWaited) {
    val userInput =(readLine() + '\n')
    val inputCodes = userInput  
      .map(_.toInt)
      .toList

    println(s"Your input is $userInput")


    comp = Computer.run(comp.copy(input=inputCodes, output=List(), status=Status.Ok))
    println(comp.output.map(_.toInt.toChar).mkString)
  }

