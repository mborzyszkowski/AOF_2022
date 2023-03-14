package mb
package distressSignal

trait RecursiveList

object RecursiveList {

  case class ListElement(elements: Seq[RecursiveList]) extends RecursiveList

  case class ValueElement(value: Value) extends RecursiveList
}