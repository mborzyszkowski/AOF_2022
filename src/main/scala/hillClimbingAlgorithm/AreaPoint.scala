package mb
package hillClimbingAlgorithm

import SourceWrapper.withSource

case class AreaPoint(x: Int, y: Int, highType: HighType)

object AreaPoint {

  def parseFromFile(filePath: String): Seq[Seq[AreaPoint]] =
    withSource(filePath) {
      _.getLines()
        .map(_.trim)
        .zipWithIndex
        .map { case (line, y) => toAreaPoints(line, y) }
        .toSeq
    }

  def toAreaPoints(line: String, y: Int): Seq[AreaPoint] =
    line.split("")
      .zipWithIndex
      .map { case (highType, x) => AreaPoint(x, y, HighType.parse(highType)) }
}