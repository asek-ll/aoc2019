import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day25Test {
  @Test def test(): Unit = {
    var program = fromPuzzleInput(25).toIntCode;

    assertEquals(16810049, Day25.part1(program))
  }
}
