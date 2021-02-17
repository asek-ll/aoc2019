import org.junit.Test
import org.junit.Assert._

import utils._

class Day18Test {
  @Test def test(): Unit = {

    assertEquals(8, Day18.part1(List(
      "#########",
      "#b.A.@.a#",
      "#########",
      )))

    assertEquals(86, Day18.part1(List(
      "########################",
      "#f.D.E.e.C.b.A.@.a.B.c.#",
      "######################.#",
      "#d.....................#",
      "########################",
      )))

    assertEquals(132, Day18.part1(List(
      "########################",
      "#...............b.C.D.f#",
      "#.######################",
      "#.....@.a.B.c.d.A.e.F.g#",
      "########################",
      )))

    val a = List(
      "#################",
      "#i.G..c...e..H.p#",
      "########.########",
      "#j.A..b...f..D.o#",
      "########@########",
      "#k.E..a...g..B.n#",
      "########.########",
      "#l.F..d...h..C.m#",
      "#################",
      )
    assertEquals(136, Day18.part1(a))

    val day18map = fromPuzzleInput(18)
    assertEquals(3270, Day18.part1(day18map))

    val b = List(
      "#######",
      "#a.#Cd#",
      "##...##",
      "##.@.##",
      "##...##",
      "#cB#Ab#",
      "#######",
      )

    assertEquals(8, Day18.part2(b))

    assertEquals(72, Day18.part2(List(
      "#############",
      "#g#f.D#..h#l#",
      "#F###e#E###.#",
      "#dCba...BcIJ#",
      "#####.@.#####",
      "#nK.L...G...#",
      "#M###N#H###.#",
      "#o#m..#i#jk.#",
      "#############",
      )))

    assertEquals(1628, Day18.part2(day18map))
  }
}
