package mb
package rucksackReorganization

case class Compartment(items: String)

object Compartment {

  lazy val itemsToPriorityMap: Map[Char, Int] =
    (('a' to 'z') ++ ('A' to 'Z'))
      .zipWithIndex
      .map { case (item, index) => item -> (index + 1) }
      .toMap

  def itemToPriority(item: Char): Option[Int] =
    itemsToPriorityMap.get(item)
}