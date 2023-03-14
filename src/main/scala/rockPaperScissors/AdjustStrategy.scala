package mb
package rockPaperScissors

import rockPaperScissors.FightResult.{Draw, Lost, Win}

case class AdjustStrategy(opponentGesture: Gesture, fightResult: FightResult) extends Strategy {

  def calculateScore(): Int =
    fightResult.score + adjustGestureFromOpponent().score

  def adjustGestureFromOpponent(): Gesture =
    fightResult match {
      case Win => opponentGesture.looseWithGesture
      case Lost => opponentGesture.defeatsGesture
      case Draw => opponentGesture.draftWithGesture
    }
}

object AdjustStrategy {

  def parseFromFile(filePath: String): Seq[Strategy] =
    Strategy.parseFromFile(filePath)(parseSymbols)

  def parseSymbols(symbols: Array[String]): Option[AdjustStrategy] =
    for {
      opponentGesture <- Gesture.parse(symbols(0))
      fightResult <- FightResult.parse(symbols(1))
    } yield AdjustStrategy(opponentGesture, fightResult)
}
