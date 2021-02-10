package Day16
import utils._

type Signal = List[Int]
val PATTERN = List(0, 1, 0, -1)

class Pattern(val n: Int):
  def get(i: Int): Int =
    PATTERN(((i+1) / n) % PATTERN.size)

  

  def apply(s: Signal): Int =
    math.abs(s.indices
      .map(i => s(i) * get(i))
      .reduce((i, b) => (i + b))) % 10


def parseSignal(lines: List[String]): Signal =
  lines(0)
    .map(_.asDigit)
    .toList


def phases(s: Signal, count: Int): Signal =
  val patterns = (1 to s.size)
    .map(Pattern(_))
    .toList
  
  phaseWhile(s, patterns, count)
  

def phaseWhile(s: Signal, patterns: List[Pattern], count: Int): Signal =
  count match {
    case 0 => s
    case 1 => phase(s, patterns)
    case x => phaseWhile(phase(s, patterns), patterns, x-1)
  }


def phase(s: Signal, patterns: List[Pattern]): Signal =
  patterns
    .map(p => p(s))
    .toList

def part1(lines: List[String]): Int =
   
  val signal = parseSignal(lines)

  phases(signal, 100)
    .take(8)
    .foldLeft(0)((s, x) => s * 10 + x)


def fastSim(s: Signal): Signal =
  var buff = 0
  s
    .reverse
    .map(x => {
      buff = (x + buff) % 10
      buff
    })
      .reverse

def fastSimUntil(s: Signal, count: Int): Signal =
  count match {
    case 0 => s
    case 1 => fastSim(s)
    case x => fastSimUntil(fastSim(s), x - 1)
  }

def simTail(s: Signal, offset: Int, count: Int): Signal =
  fastSimUntil(s.drop(offset), count)

def part2(lines: List[String]): Int =
  val signal = parseSignal(lines)

  val offset = signal
    .take(7)
    .foldLeft(0)((s, x) => s * 10 + x)

  val end = signal.size * 10_000

  val ns = (offset to end)
    .map(x => signal(x%signal.size))
    .toList

  fastSimUntil(ns, 100)
    .take(8)
    .foldLeft(0)((s, x) => s * 10 + x)
