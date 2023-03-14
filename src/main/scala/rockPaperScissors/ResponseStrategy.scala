package mb
package rockPaperScissors

import rockPaperScissors.FightResult.{Draw, Lost, Win}
import rockPaperScissors.Gesture.parse

case class ResponseStrategy(playerGesture: Gesture, opponentGesture: Gesture) extends Strategy {

  def calculateScore(): Int =
    playerGesture.score + calculateFightScore()

  private def calculateFightScore(): Int =
    getFightResult(this).score

  private def getFightResult(strategy: ResponseStrategy): FightResult =
    strategy match {
      case ResponseStrategy(playerGesture, opponentGesture) if playerGesture.defeatsGesture == opponentGesture => Win
      case ResponseStrategy(playerGesture, opponentGesture) if playerGesture.looseWithGesture == opponentGesture => Lost
      case ResponseStrategy(playerGesture, opponentGesture) if playerGesture.draftWithGesture == opponentGesture => Draw
    }
}

object ResponseStrategy {

  def parseFromFile(filePath: String): Seq[Strategy] =
    Strategy.parseFromFile(filePath)(parseSymbols)

  def parseSymbols(symbols: Array[String]): Option[Strategy] =
    for {
      opponentGesture <- parse(symbols(0))
      playerGesture <- parse(symbols(1))
    } yield ResponseStrategy(playerGesture, opponentGesture)
}
