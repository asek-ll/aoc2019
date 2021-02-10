import org.junit.Test
import org.junit.Assert._

import Day16._
import utils._

class Day16Test {
  @Test def test(): Unit = {

    assertEquals(List(1,0,-1,0,1,0,-1,0,1,0), (0 to 9).map(Pattern(1).get(_)).toList)
    assertEquals(List(0,1,1,0,0,-1,-1,0,0,1), (0 to 9).map(Pattern(2).get(_)).toList)
    assertEquals(List(0,0,1,1,1,0,0,0,-1,-1), (0 to 9).map(Pattern(3).get(_)).toList)

    val s = parseSignal(List("12345678"))
    assertEquals("48226158", phases(s, 1).mkString)
    assertEquals("34040438", phases(s, 2).mkString)
    assertEquals("03415518", phases(s, 3).mkString)
    assertEquals("01029498", phases(s, 4).mkString)


    assertEquals(73745418, part1(List("19617804207202209144916044189917")))
    assertEquals(24176176, part1(List("80871224585914546619083218645595")))
    assertEquals(52432133, part1(List("69317163492948606335995924319873")))

    assertEquals(78009100, part1(fromPuzzleInput(16)))

    val longS = parseSignal(List("12345678123456781234567812345678123456781234567812345678123456781234567812345678"))
    assertEquals(phases(longS, 10).drop(50), simTail(longS, 50, 10))

    assertEquals(84462026, part2(List("03036732577212944063491565474664")))
    assertEquals(78725270, part2(List("02935109699940807407585447034323")))
    assertEquals(53553731, part2(List("03081770884921959731165446850517")))

    assertEquals(37717791, part2(fromPuzzleInput(16)))

  }
}
