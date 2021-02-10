import org.junit.Test
import org.junit.Assert._

import computer._
import utils._

class ComputerTest {
  @Test def testBaseInstruction(): Unit = {
    assertEquals(List(2,0,0,0,99), Computer.run(List(1,0,0,0,99)).program)
    assertEquals(List(2,3,0,6,99), Computer.run(List(2,3,0,3,99)).program)
    assertEquals(List(2,4,4,5,99,9801), Computer.run(List(2,4,4,5,99,0)).program)
    assertEquals(List(30,1,1,4,2,5,6,0,99), Computer.run(List(1,1,1,4,99,5,6,0,99)).program)
  }

  @Test def testIOInstruction(): Unit = {
    assertEquals(List(42), Computer.run(List(3,0,4,0,99), List(42)).output)
    assertEquals(List(304099), Computer.run(List(3,0,4,0,99), List(304099)).output)

    assertEquals(Status.Halted, Computer.run(List(1101,100,-1,4,0)).status)
    assertEquals(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 13087969), Computer.run(fromPuzzleInput(5).toIntCode, List(1)).output)
    

  }

  @Test def testJumpIfInstructions(): Unit = {

    assertEquals(1, Computer.run(List(3,9,8,9,10,9,4,9,99,-1,8), List(8)).output.head)
    assertEquals(0, Computer.run(List(3,9,8,9,10,9,4,9,99,-1,8), List(-4)).output.head)
    assertEquals(0, Computer.run(List(3,9,8,9,10,9,4,9,99,-1,8), List(10)).output.head)

    assertEquals(0, Computer.run(List(3,9,7,9,10,9,4,9,99,-1,8), List(8)).output.head)
    assertEquals(1, Computer.run(List(3,9,7,9,10,9,4,9,99,-1,8), List(-4)).output.head)
    assertEquals(0, Computer.run(List(3,9,7,9,10,9,4,9,99,-1,8), List(10)).output.head)

    assertEquals(1, Computer.run(List(3,3,1108,-1,8,3,4,3,99), List(8)).output.head)
    assertEquals(0, Computer.run(List(3,3,1108,-1,8,3,4,3,99), List(-4)).output.head)
    assertEquals(0, Computer.run(List(3,3,1108,-1,8,3,4,3,99), List(10)).output.head)

    assertEquals(0, Computer.run(List(3,3,1107,-1,8,3,4,3,99), List(8)).output.head)
    assertEquals(1, Computer.run(List(3,3,1107,-1,8,3,4,3,99), List(-4)).output.head)
    assertEquals(0, Computer.run(List(3,3,1107,-1,8,3,4,3,99), List(10)).output.head)


    val cmp8 = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

    assertEquals(1000, Computer.run(cmp8, List(8)).output.head)
    assertEquals(999, Computer.run(cmp8, List(-4)).output.head)
    assertEquals(1001, Computer.run(cmp8, List(10)).output.head)

    assertEquals(List(14110739), Computer.run(fromPuzzleInput(5).toIntCode, List(5)).output)
  }

  @Test def testRelativeBaseAndBigNumbers(): Unit = {

    val quine = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    assertEquals(quine, Computer.run(quine).output)
    assertEquals(16, Computer.run(List(1102,34915192,34915192,7,4,7,99,0)).output.head.toString.size)
    assertEquals(1125899906842624L, Computer.run(List(104,1125899906842624L,99)).output.head)

    assertEquals(List(2465411646L), Computer.run(fromPuzzleInput(9).toIntCode, List(1)).output)
    assertEquals(List(69781), Computer.run(fromPuzzleInput(9).toIntCode, List(2)).output)
  }
}
