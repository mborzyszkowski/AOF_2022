package mb
package hillClimbingAlgorithm

object HillClimbingAlgorithm extends App {

  val filePath = "src/main/scala/hillClimbingAlgorithm/input.txt"

  val area = AreaPoint.parseFromFile(filePath)

  val startingPoint: AreaPoint = area.flatMap(_.filter(_.highType == HighType.start)).head

  val pathFinder = PathFinder(area, startingPoint)

  val shortestPathToTheBestSignalLocation = pathFinder.findOptimalPathToTheBestSignalLocation().get

  println(s"Optimal path of length ${shortestPathToTheBestSignalLocation.points.size - 1}")

  pathFinder.printPath(shortestPathToTheBestSignalLocation)

  val allStartingPointsPaths =
    area.flatMap(_.filter(_.highType.elevation == 'a'))
      .map(PathFinder(area, _))
      .map(pathFinder => pathFinder.findOptimalPathToTheBestSignalLocation())
      .filter(_.nonEmpty)
      .map(_.get)

  println()
  println(s"Possible starting points ${allStartingPointsPaths.size}")

  val shortestPathFromStartingPoints = allStartingPointsPaths.minBy(_.points.size)

  println()
  println(s"Shortest path from all starting points of length ${shortestPathFromStartingPoints.points.size - 1}")
  pathFinder.printPath(shortestPathFromStartingPoints)
}
