package mb
package rucksackReorganization

import rucksackReorganization.Compartment.itemToPriority
import rucksackReorganization.ElvesTrio.groupToElvesTrios

object RucksackReorganization extends App {

  val filePath = "src/main/scala/rucksackReorganization/input.txt"

  val rucksacks = Rucksack.parseFromFile(filePath)

  val itemsInsideRucksackPriorities =
    rucksacks
      .map(_.itemThatExistsInBothCompartments)
      .map(itemToPriority)
      .filter(_.nonEmpty)
      .map(_.get)

  println(s"Sum of priorities of items that exists in both compartments inside rucksack ${itemsInsideRucksackPriorities.sum}")

  val elvesTrios =
    groupToElvesTrios(rucksacks)

  val itemsTrioPriorities =
    elvesTrios
      .map(_.itemThatExistsInAllRucksack)
      .map(_.head)
      .map(itemToPriority)
      .filter(_.nonEmpty)
      .map(_.get)

  println(s"Sum of priorities of items that exists in all rucksacks in their group ${itemsTrioPriorities.sum}")
}
