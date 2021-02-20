package Day21

import computer._

def getProgramResult(program: IntCode, commands: List[String]): Int = 
  val res = AsciiComputer.run(program, commands)

  res match {
    case Left(out) => {
      println(out)
      -1
    }
    case Right(out) => out.last.toInt
  }

def part1(program: IntCode): Int = 
  getProgramResult(program, List(
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "WALK",
  ))


def part2(program: IntCode): Int = 
  getProgramResult(program, List(
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "NOT E T",
    "NOT T T",
    "OR H T",
    "AND T J",
    "RUN",
  ))


