import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day21Test {
  @Test def test(): Unit = {
    var program = fromPuzzleInput(21).toIntCode;

    assertEquals(19361414, Day21.part1(program))

    assertEquals(1139205618, Day21.part2(program))
  }
}
