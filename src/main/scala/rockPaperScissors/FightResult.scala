package mb
package rockPaperScissors

sealed abstract class FightResult(val symbol: String, val score: Int)

object FightResult {
  object Win extends FightResult("Z", 6)
  object Draw extends FightResult("Y", 3)
  object Lost extends FightResult("X", 0)

  lazy val fightResultMap: Map[String, FightResult] =
    Seq(Win, Draw, Lost)
      .map(fightResult => fightResult.symbol -> fightResult)
      .toMap

  def parse(symbol: String): Option[FightResult] =
    fightResultMap.get(symbol)
}