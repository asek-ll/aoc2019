import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day19Test {
  @Test def test(): Unit = {
    assertEquals(169, Day19.part1(fromPuzzleInput(19).toIntCode))
    assertEquals(7001134, Day19.part2(fromPuzzleInput(19).toIntCode))
  }
}
