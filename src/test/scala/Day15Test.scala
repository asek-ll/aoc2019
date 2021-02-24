import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day15Test {
  @Test def test(): Unit = {
    assertEquals(228, Day15.part1(fromPuzzleInput(15).toIntCode))
    assertEquals(348, Day15.part2(fromPuzzleInput(15).toIntCode))
  }
}
