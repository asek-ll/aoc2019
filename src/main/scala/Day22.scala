package Day22

import scala.collection.mutable.ArrayBuffer

trait Technique:
  def apply(p: Formula, m: Long): Formula

case object IntoNewStack extends Technique:
  def apply(p: Formula, m: Long): Formula = Formula(-p.mul + m, -p.free-1 + m)

case class Cut(val n: Int) extends Technique:
  def apply(p: Formula, m: Long): Formula = p.copy(free = (p.free - n + m) % m)

case class WithIncrement(val n: Int) extends Technique:
  def apply(p: Formula, m: Long): Formula = Formula((p.mul * n) % m, (p.free * n) % m)


def parseTechniques(lines: List[String]): Seq[Technique] = 
  lines
    .map(l => {
      val parts = l.split(" ")
      parts.size match {
        case 2 => Cut(parts.last.toInt)
        case 4 if parts(1) == "into" => IntoNewStack
        case 4 if parts(1) == "with" => WithIncrement(parts.last.toInt)
      }
    })

case class Formula(val mul: BigInt, val free: BigInt):
  def apply(x: Long, m: Long): Long = (x * mul + free).mod(m).toLong

  def revert(x: Long, m: Long): Long =
    var dop = mul.modPow(m-2, m)
    ((x - free) * dop).mod(m).toLong

  def times(x: Long, m: Long): Formula =
    val newMul = mul.modPow(x, m)

    val up = (m + 1 - mul.modPow(x, m))
    val down = m + 1 - mul
    val dop = down.modPow(m-2, m)

    val gs = free * up * dop

    val newFree = gs.mod(m) 

    Formula(newMul, newFree)

def part1(lines: List[String], cardNo: Long = 2019, cardNums: Long = 10007): Long = 
  val ps = parseTechniques(lines)
  val f = ps.foldLeft(Formula(1, 0))((p, t) => t(p, cardNums))

  f(cardNo, cardNums)

def part2(lines: List[String], cardNo: Long = 2020, cardNums: Long = 119315717514047): Long =
  val ps = parseTechniques(lines)

  val f = ps.foldLeft(Formula(1, 0))((p, t) => t(p, cardNums))

  val repeatedFormula = f.times(101741582076661L, cardNums)
  repeatedFormula.revert(cardNo, cardNums)
