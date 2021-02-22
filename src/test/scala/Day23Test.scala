import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day23Test {
  @Test def test(): Unit = {
    var program = fromPuzzleInput(23).toIntCode;

    assertEquals(23213, Day23.part1(program))
    assertEquals(17874, Day23.part2(program))
  }
}
