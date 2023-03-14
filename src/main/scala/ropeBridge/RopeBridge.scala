package mb
package ropeBridge

object RopeBridge extends App {

  val filePath = "src/main/scala/ropeBridge/input.txt"

  val movements = Move.parseFromFile(filePath)

  val (climberAfterMovements, visitedPositions) = Climber().makeMoves(movements)

  println(s"Climber visited ${visitedPositions.size} positions and is at H(${climberAfterMovements.headPosition}) T(${climberAfterMovements.tail})")

  val (climberAfterMovementsWithNineKnots, visitedPositionsWithNineKnots) = Climber(9).makeMoves(movements)

  println(s"Climber visited ${visitedPositionsWithNineKnots.size} positions and is at H(${climberAfterMovementsWithNineKnots.headPosition}) T(${climberAfterMovementsWithNineKnots.tail})")
}
