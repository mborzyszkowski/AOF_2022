package mb
package distressSignal

import distressSignal.RecursiveList.{ListElement, ValueElement}
import distressSignal.RecursiveListZipper.{ListElementZipper, ValueElementZipper}

import scala.annotation.tailrec

trait RecursiveListZipper {

  def left: Seq[RecursiveList]

  def up: Option[RecursiveListZipper]

  def right: Seq[RecursiveList]

  def goDown: RecursiveListZipper

  def goDownLeft: RecursiveListZipper

  def goDownRight: RecursiveListZipper

  def goUp: RecursiveListZipper =
    up.get match {
      case ListElementZipper(_, parentLeft, parentUp, parentRight) =>
        ListElementZipper((left :+ unzipCurrent) ++ right, parentLeft, parentUp, parentRight)
    }

  def goLeft: RecursiveListZipper =
    left.head match {
      case ListElement(siblingElements) => ListElementZipper(siblingElements, left.tail, up, right :+ unzipCurrent)
      case ValueElement(siblingValue) => ValueElementZipper(siblingValue, left.tail, up, right :+ unzipCurrent)
    }

  def goRight: RecursiveListZipper =
    right.head match {
      case ListElement(siblingElements) => ListElementZipper(siblingElements, left :+ unzipCurrent, up, right.tail)
      case ValueElement(siblingValue) => ValueElementZipper(siblingValue, left :+ unzipCurrent, up, right.tail)
    }

  def insertLeft(recursiveList: RecursiveList): RecursiveListZipper

  def insertRight(recursiveList: RecursiveList): RecursiveListZipper

  def insertDown(recursiveList: RecursiveList): RecursiveListZipper

  def replaceCurrent(recursiveList: RecursiveList): RecursiveListZipper =
    recursiveList match {
      case ListElement(givenElements) => ListElementZipper(givenElements, left, up, right)
      case ValueElement(givenValue) => ValueElementZipper(givenValue, left, up, right)
    }

  def replaceCurrent(recursiveList: RecursiveListZipper): RecursiveListZipper =
    recursiveList match {
      case ListElementZipper(givenElements, _, _, _) => ListElementZipper(givenElements, left, up, right)
      case ValueElementZipper(givenValue, _, _, _) => ValueElementZipper(givenValue, left, up, right)
    }

  def unzipCurrent: RecursiveList

  @tailrec
  final def unzipAll: ListElement =
    this match {
      case ListElementZipper(elements, _, up, _) if up.isEmpty => ListElement(elements)
      case ListElementZipper(_, _, _, _) => goUp.unzipAll
      case ValueElementZipper(_, _, _, _) => goUp.unzipAll
    }
}

object RecursiveListZipper {

  case class ListElementZipper(elements: Seq[RecursiveList], left: Seq[RecursiveList], up: Option[RecursiveListZipper], right: Seq[RecursiveList]) extends RecursiveListZipper {

    override def goDown: RecursiveListZipper =
      elements.head match {
        case ListElement(childElements) => ListElementZipper(childElements, Seq(), Option(this), elements.tail)
        case ValueElement(value) => ValueElementZipper(value, Seq(), Option(this), elements.tail)
      }

    override def goDownLeft: RecursiveListZipper = goDown

    override def goDownRight: RecursiveListZipper =
      elements.last match {
        case ListElement(childElements) => ListElementZipper(childElements, elements.dropRight(1), Option(this), Seq())
        case ValueElement(value) => ValueElementZipper(value, elements.dropRight(1), Option(this), Seq())
      }

    override def insertLeft(recursiveList: RecursiveList): RecursiveListZipper =
      ListElementZipper(elements, left :+ recursiveList, up, right)

    override def insertRight(recursiveList: RecursiveList): RecursiveListZipper =
      ListElementZipper(elements, left, up, right :+ recursiveList)

    override def insertDown(recursiveList: RecursiveList): RecursiveListZipper =
      ListElementZipper(elements :+ recursiveList, left, up, right)

    override def unzipCurrent: RecursiveList =
      ListElement(elements)
  }

  object ListElementZipper {

    def apply(element: Seq[RecursiveList]): ListElementZipper =
      ListElementZipper(element, Seq(), None, Seq())
  }


  case class ValueElementZipper(value: Value, left: Seq[RecursiveList], up: Option[RecursiveListZipper], right: Seq[RecursiveList]) extends RecursiveListZipper {

    override def goDown: RecursiveListZipper =
      throw new Error(s"Value element can not have children")

    override def goDownLeft: RecursiveListZipper = goDown

    override def goDownRight: RecursiveListZipper = goDown

    override def insertLeft(recursiveList: RecursiveList): RecursiveListZipper =
      ValueElementZipper(value, left :+ recursiveList, up, right)

    override def insertRight(recursiveList: RecursiveList): RecursiveListZipper =
      ValueElementZipper(value, left, up, right :+ recursiveList)

    override def insertDown(recursiveList: RecursiveList): RecursiveListZipper =
      throw new Error(s"Value element can not have children")

    override def unzipCurrent: RecursiveList =
      ValueElement(value)
  }
}