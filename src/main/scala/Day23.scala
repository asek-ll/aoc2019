package Day23

import computer._

type Comps = IndexedSeq[State]

def step(comps: Comps): Int =
  val queueRaw = comps
    .flatMap(_.output.grouped(3))
    .groupBy(_.head)

  val queue = queueRaw
    .view.mapValues(ps => ps.flatMap(p => p.tail).toList)
    .toMap

  if queue.contains(255) then return queue(255)(1).toInt

  val emptyInput = List(-1)

  val newComps = comps.indices
    .map(a => Computer.run(comps(a).copy(status=Status.Ok, input=queue
        .get(a).getOrElse(emptyInput), output=List())))


  step(newComps)



def part1(codes: IntCode): Int =

  val computers = (0 to 49)
    .map(a => Computer.run(codes, List(a))) 

  step(computers)

