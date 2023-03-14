package mb
package rucksackReorganization

case class ElvesTrio(first: Rucksack, second: Rucksack, third: Rucksack) {

  def itemThatExistsInAllRucksack: String =
    for {
      firstItem <- first.items
      secondItem <- second.items
      thirdItem <- third.items
      if firstItem == secondItem && secondItem == thirdItem
    } yield firstItem
}

object ElvesTrio {

  def groupToElvesTrios(rucksacks: Seq[Rucksack]): Seq[ElvesTrio] =
    rucksacks.grouped(3)
      .map(r => ElvesTrio(r.head, r.tail.head, r.tail.tail.head))
      .toSeq
}
