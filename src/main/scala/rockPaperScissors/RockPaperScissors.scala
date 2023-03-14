package mb
package rockPaperScissors

object RockPaperScissors extends App {

  val filePath = "src/main/scala/rockPaperScissors/input.txt"


  val responseStrategies = ResponseStrategy.parseFromFile(filePath)

  val responseStrategiesFightScores = responseStrategies.map(_.calculateScore())

  println(s"Sum of response strategies scores: ${responseStrategiesFightScores.sum}")


  val adjustStrategies = AdjustStrategy.parseFromFile(filePath)

  val adjustStrategiesFightScores = adjustStrategies.map(_.calculateScore())

  println(s"Sum of response strategies scores: ${adjustStrategiesFightScores.sum}")
}