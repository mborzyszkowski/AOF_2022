package mb
package hillClimbingAlgorithm

import scala.annotation.tailrec

case class PathFinder(area: Seq[Seq[AreaPoint]], startingPoint: AreaPoint) {

  lazy val theBestSignalLocationPoint: AreaPoint = area.flatMap(_.filter(_.highType == HighType.theBestSignalLocation)).head

  def findOptimalPathToTheBestSignalLocation(): Option[Path] =
    findOptimalPathToTheBestSignalLocation(Seq(Path(Seq(startingPoint))), Set())

  @tailrec
  final def findOptimalPathToTheBestSignalLocation(possiblePaths: Seq[Path], allVisitedPositions: Set[AreaPoint]): Option[Path] =
    possiblePaths match {
      case path +: otherPaths =>
        val nextPossiblePoints = calculatePossibleNextSteps(path).filterNot(allVisitedPositions)

        if (nextPossiblePoints.contains(theBestSignalLocationPoint))
          Some(Path(path.points :+ theBestSignalLocationPoint))
        else
          findOptimalPathToTheBestSignalLocation(otherPaths ++ nextPossiblePoints.map(point => Path(path.points :+ point)), allVisitedPositions ++ nextPossiblePoints)
      case _ => None
    }

  def getPoint(x: Int, y: Int): Option[AreaPoint] =
    area.lift(y) match {
      case Some(row) => row.lift(x)
      case None => None
    }

  def calculatePossibleNextSteps(path: Path): Seq[AreaPoint] = {
    val currentPoint = path.points.last

    Seq(
      getPoint(currentPoint.x + 1, currentPoint.y),
      getPoint(currentPoint.x - 1, currentPoint.y),
      getPoint(currentPoint.x, currentPoint.y + 1),
      getPoint(currentPoint.x, currentPoint.y - 1)
    ).filter(_.nonEmpty)
      .map(_.get)
      .filterNot(path.points.contains)
      .filter(_.highType.height.abs - currentPoint.highType.height <= 1)
  }

  def printPath(path: Path): Unit = {
    val maxX = path.points.map(_.x).max
    val maxY = path.points.map(_.y).max

    for {
      y <- 0 to maxY
      x <- 0 to maxX
    } {
      val point = path.points.find(point => point.x == x && point.y == y)
      val signToPrint = point.map(_.highType.name).getOrElse("_")

      if (x == maxX)
        println(signToPrint)
      else
        print(signToPrint)
    }
  }
}
