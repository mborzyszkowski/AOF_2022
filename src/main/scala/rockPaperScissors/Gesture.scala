package mb
package rockPaperScissors

import rockPaperScissors.Gesture.{Paper, Rock, Scissors}

sealed abstract class Gesture(val allowedSymbols: Seq[String], val score: Int) {

  def defeatsGesture: Gesture =
    this match {
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper
    }

  def looseWithGesture: Gesture =
    this match {
      case Rock => Paper
      case Paper => Scissors
      case Scissors => Rock
    }

  def draftWithGesture: Gesture = this
}

object Gesture {
  object Rock extends Gesture(Seq("A", "X"), 1)
  object Paper extends Gesture(Seq("B", "Y"), 2)
  object Scissors extends Gesture(Seq("C", "Z"), 3)

  lazy val gestureMap: Map[String, Gesture] =
    Seq(Rock, Paper, Scissors)
      .flatMap(gesture => gesture.allowedSymbols.map(symbol => symbol -> gesture))
      .toMap

  def parse(symbol: String): Option[Gesture] =
    gestureMap.get(symbol)
}
