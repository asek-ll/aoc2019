import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.ArrayBuffer

import utils._

class Day22Test {

  def testDeck(commands: List[String]): List[Int] = {
    val res = ArrayBuffer.fill(10)(0)
    (0 until 10)
      .map(p => {
        res(Day22.part1(commands, p, 10).toInt) = p
      })

      res.toList 
  }


  @Test def test(): Unit = {
  
    assertEquals(List(0, 3, 6, 9, 2, 5, 8, 1, 4, 7), testDeck(List(
      "deal with increment 7",
      "deal into new stack",
      "deal into new stack",
      )))

    assertEquals(List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6), testDeck(List(
      "cut 6",
      "deal with increment 7",
      "deal into new stack",
      )))

    assertEquals(List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6), testDeck(List(
      "deal into new stack",
      "cut -2",
      "deal with increment 7",
      "cut 8",
      "cut -4",
      "deal with increment 7",
      "cut 3",
      "deal with increment 9",
      "deal with increment 3",
      "cut -1",
      )))

    var lines = fromPuzzleInput(22)
    assertEquals(7395, Day22.part1(lines))

    assertEquals(32376123569821L, Day22.part2(lines))
  }
}
