package mb
package distressSignal

import distressSignal.RecursiveList.{ListElement, ValueElement}
import distressSignal.RecursiveListZipper.{ListElementZipper, ValueElementZipper}

import scala.annotation.tailrec

case class PacketsPairsProcessor() {

  def isTheRightOrder(leftPacket: Packet, rightPacket: Packet): Either[String, (RecursiveList, RecursiveList)] = {
    val leftZipper = ListElementZipper(Seq(leftPacket.recursiveList))
    val rightZipper = ListElementZipper(Seq(rightPacket.recursiveList))

    checkTheRightOrder(leftZipper, rightZipper)
  }

  @tailrec
  final def checkTheRightOrder(leftZipper: RecursiveListZipper, rightZipper: RecursiveListZipper): Either[String, (RecursiveList, RecursiveList)] =
    oneStepCheck(leftZipper, rightZipper) match {
      case Right((left, right, true)) => Right((left.unzipAll, right.unzipAll))
      case Right((left, right, false)) => checkTheRightOrder(left, right)
      case Left(value) => Left(value)
    }

  def oneStepCheck(leftZipper: RecursiveListZipper, rightZipper: RecursiveListZipper): Either[String, (RecursiveListZipper, RecursiveListZipper, Boolean)] =
    (leftZipper, rightZipper) match {
      case (left: ValueElementZipper, right: ValueElementZipper) =>
        if (left.value.value < right.value.value)
          Right(left, right, true)
        else if (left.value.value > right.value.value)
          Left(s"Left ${left.value} is grater then right ${right.value}")
        else
          goToTheNextItem(left, right)
      case (left: ListElementZipper, right: ListElementZipper) =>
        if (left.elements.nonEmpty && right.elements.nonEmpty)
          Right(left.goDown, right.goDown, false)
        else if (left.elements.isEmpty && right.elements.nonEmpty)
          Right(left, right, true)
        else if (left.elements.nonEmpty && right.elements.isEmpty)
          Left(s"Right run out of items $right")
        else
          goToTheNextItem(left, right)
      case (left: ListElementZipper, right: ValueElementZipper) =>
        Right(left, right.replaceCurrent(ListElement(Seq(ValueElement(right.value)))), false)
      case (left: ValueElementZipper, right: ListElementZipper) =>
        Right(left.replaceCurrent(ListElement(Seq(ValueElement(left.value)))), right, false)
    }

  def goToTheNextItem(left: RecursiveListZipper, right: RecursiveListZipper): Either[String, (RecursiveListZipper, RecursiveListZipper, Boolean)] =
    tryToGoRightOrElse(left, right, (left, right) => goUpToTheNextItem(left, right))

  @tailrec
  final def goUpToTheNextItem(left: RecursiveListZipper, right: RecursiveListZipper): Either[String, (RecursiveListZipper, RecursiveListZipper, Boolean)] =
    if (left.right.isEmpty && right.right.isEmpty && left.up.nonEmpty && right.up.nonEmpty)
      goUpToTheNextItem(left.goUp, right.goUp)
    else if (left.right.isEmpty && right.right.isEmpty && left.up.isEmpty && right.up.isEmpty)
      Right(left, right, true)
    else
      tryToGoRightOrElse(left, right, (_, _) => throw new Error("Sth is wrong or missing"))

  def tryToGoRightOrElse(left: RecursiveListZipper, right: RecursiveListZipper, orElseFunction: (RecursiveListZipper, RecursiveListZipper) => Either[String, (RecursiveListZipper, RecursiveListZipper, Boolean)]): Either[String, (RecursiveListZipper, RecursiveListZipper, Boolean)] = {
    if (left.right.nonEmpty && right.right.nonEmpty)
      Right(left.goRight, right.goRight, false)
    else if (left.right.isEmpty && right.right.nonEmpty)
      Right(left, right, true)
    else if (left.right.nonEmpty && right.right.isEmpty)
      Left(s"Right run out of items $right")
    else
      orElseFunction(left, right)
  }
}
