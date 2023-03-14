package mb
package distressSignal

import distressSignal.RecursiveList.{ListElement, ValueElement}

object DistressSignal extends App {

  val filePath = "src/main/scala/distressSignal/input.txt"

  val packetsPairs = Packet.parseFromFile(filePath)

  val packetsPairsProcessor = PacketsPairsProcessor()

  val checkResults =
    packetsPairs.zipWithIndex
      .map(pair => (pair._2 + 1, packetsPairsProcessor.isTheRightOrder(pair._1._1, pair._1._2)))

  println(checkResults.filter(_._2.isRight).map(_._1).sum)


  println()

  val allPackets = packetsPairs.flatMap(pair => Seq(pair._1, pair._2))

  val dividerPackets = Seq(
    Packet(ListElement(Seq(ListElement(Seq(ValueElement(Value(2))))))),
    Packet(ListElement(Seq(ListElement(Seq(ValueElement(Value(6)))))))
  )

  val packetsToProcess = dividerPackets ++ allPackets

  val sortedPackets = packetsToProcess.sortWith { case (firstPacket: Packet, secondPacket: Packet) => packetsPairsProcessor.isTheRightOrder(firstPacket, secondPacket).isRight }

  val firstDividerPosition = sortedPackets.indexOf(dividerPackets.head) + 1
  val secondDividerPosition = sortedPackets.indexOf(dividerPackets.tail.head) + 1

  println(firstDividerPosition)
  println(secondDividerPosition)
  println(firstDividerPosition * secondDividerPosition)
}
