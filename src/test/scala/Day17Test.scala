import org.junit.Test
import org.junit.Assert._

import utils._
import computer._

class Day17Test {
  @Test def test(): Unit = {

    val testMap = Array(
      "..#..........",
      "..#..........",
      "#######...###",
      "#.#...#...#.#",
      "#############",
      "..#...#...#..",
      "..#####...^..",
      )

    assertEquals(76, Day17.getAligmentParameter(testMap))

    assertEquals(4220, Day17.part1(fromPuzzleInput(17).toIntCode))

    val testMap2 = Array(
      "#######...#####",
      "#.....#...#...#",
      "#.....#...#...#",
      "......#...#...#",
      "......#...###.#",
      "......#.....#.#",
      "^########...#.#",
      "......#.#...#.#",
      "......#########",
      "........#...#..",
      "....#########..",
      "....#...#......",
      "....#...#......",
      "....#...#......",
      "....#####......",
      )

    val program = Day17.getRouteProgram(testMap2)
    val allCommands = program.mainRoute
      .flatMap(idx => program.funcs(idx))
      .mkString(",")

    assertEquals("R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2", allCommands)

    assertEquals(809736, Day17.part2(fromPuzzleInput(17).toIntCode))
  }
}
