package mb
package distressSignal

import SourceWrapper.withSource
import distressSignal.RecursiveList.{ListElement, ValueElement}
import distressSignal.RecursiveListZipper.{ListElementZipper, ValueElementZipper}

import java.lang.Integer.parseInt
import scala.annotation.tailrec

case class Packet(recursiveList: RecursiveList)

object Packet {

  def parseFromFile(filePath: String): Seq[(Packet, Packet)] =
    withSource(filePath) {
      _.getLines()
        .filter(_.nonEmpty)
        .map(_.trim)
        .grouped(2)
        .map(toPacketsPair)
        .toSeq
    }

  def toPacketsPair(lines: Seq[String]): (Packet, Packet) =
    (parsePacket(lines.head), parsePacket(lines.tail.head))

  def parsePacket(line: String): Packet =
    parsePacket(line.toSeq, ListElementZipper(Seq(), Seq(), None, Seq()))

  @tailrec
  final def parsePacket(line: Seq[Char], currentZippedList: RecursiveListZipper): Packet =
    line match {
      case '[' +: ']' +: tail                       => parsePacket(tail, currentZippedList.insertDown(ListElement(Seq())).goDownRight)
      case '[' +: tail                              => parsePacket(tail, currentZippedList.insertDown(ListElement(Seq())).goDownRight)
      case ',' +: '[' +: ']' +: tail                => parsePacket(tail, currentZippedList.insertRight(ListElement(Seq())).goRight)
      case ',' +: '[' +: tail                       => parsePacket(tail, currentZippedList.insertRight(ListElement(Seq())).goRight)
      case ']' +: tail                              => parsePacket(tail, currentZippedList.goUp)
      case number +: tail if number.isDigit         => parsePacket(tail, addNumber(currentZippedList, parseInt(number.toString)))
      case ',' +: number +: tail if number.isDigit  => parsePacket(tail, currentZippedList.insertRight(ValueElement(Value(parseInt(number.toString)))).goRight)
      case _                                        => Packet(currentZippedList.unzipAll.elements.head)
    }

  def addNumber(currentZippedList: RecursiveListZipper, number: Int): RecursiveListZipper =
    currentZippedList match {
      case ListElementZipper(_, _, _, _)        => currentZippedList.insertDown(ValueElement(Value(number))).goDownRight
      case ValueElementZipper(value,  _, _, _)  => currentZippedList.replaceCurrent(ValueElement(Value(value.value * 10 + number)))
    }
}
