package Day23

import computer._

type Comps = IndexedSeq[State]

def part1Sim(comps: Comps): Int =
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


  part1Sim(newComps)

case class SimState(
  val comps: Comps,
  val nasPacket: Option[List[Code]] = None,
  val lastNasDelivered: Option[List[Code]] = None
)

def part2Sim(state: SimState): Int =
  val queueRaw = state.comps
    .flatMap(_.output.grouped(3))
    .groupBy(_.head)

  val queue = queueRaw
    .view.mapValues(ps => ps.flatMap(p => p.tail).toList)
    .toMap

  val currentNasPacket = queue.get(255)

  val nasPacket = currentNasPacket.map(o => o.slice(o.size-2, o.size)).orElse(state.nasPacket)

  val packetToSend = if queue.isEmpty then nasPacket else None

  val resultQueue = packetToSend.map(p => Map(0 -> p)).getOrElse(queue)

  if packetToSend.isDefined && packetToSend == state.lastNasDelivered
  then return packetToSend.get(1).toInt

  val emptyInput = List(-1)

  val newComps = state.comps.indices
    .map(a => Computer.run(state.comps(a).copy(status=Status.Ok, input=resultQueue
        .get(a).getOrElse(emptyInput), output=List())))

  part2Sim(SimState(newComps, nasPacket, packetToSend.orElse(state.lastNasDelivered)))


def part1(codes: IntCode): Int =

  val computers = (0 to 49)
    .map(a => Computer.run(codes, List(a))) 

  part1Sim(computers)


def part2(codes: IntCode): Int =

  val computers = (0 to 49)
    .map(a => Computer.run(codes, List(a))) 

  part2Sim(SimState(computers))
