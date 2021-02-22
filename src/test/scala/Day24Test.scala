import org.junit.Test
import org.junit.Assert._

import utils._

class Day24Test {

  @Test def test(): Unit = {
    assertEquals(2129920, Day24.part1(List(
      "....#",
      "#..#.",
      "#..##",
      "..#..",
      "#....",
      )))

    assertEquals(18371095, Day24.part1(fromPuzzleInput(24)))

    assertEquals(99, Day24.part2(List(
      "....#",
      "#..#.",
      "#..##",
      "..#..",
      "#....",
      ), 10))

    assertEquals(2075, Day24.part2(fromPuzzleInput(24)))
  }
}
