package mb
package rucksackReorganization

import SourceWrapper.withSource

case class Rucksack(firstCompartment: Compartment, secondCompartment: Compartment) {

  def itemThatExistsInBothCompartments: Char =
    firstCompartment.items
      .filter(item => secondCompartment.items.contains(item))
      .head

  def items: String = firstCompartment.items + secondCompartment.items
}

object Rucksack {

  def parseFromFile(filePath: String): Seq[Rucksack] =
    withSource(filePath) {
      _.getLines()
        .map(splitByHalf)
        .map(s => Rucksack(Compartment(s._1), Compartment(s._2)))
        .toSeq
    }

  private def splitByHalf(line: String): (String, String) = {
    val midIndex = line.length / 2
    (line.substring(0, midIndex), line.substring(midIndex))
  }
}