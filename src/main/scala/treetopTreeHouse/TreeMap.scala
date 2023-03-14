package mb
package treetopTreeHouse

import SourceWrapper._

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.BufferedSource
import scala.language.implicitConversions

case class TreesMap(verticalTreeMap: ListMap[Int, Seq[Int]], horizontalTreeMap: ListMap[Int, Seq[Int]]) {

  implicit def asSeq[T](input: Iterable[T]): Seq[T] = input.toSeq

  def visibleTrees: Seq[Tree] =
    for {
      verticalIndex <- verticalTreeMap.keys
      horizontalIndex <- horizontalTreeMap.keys
      if isTreeVisibleOutsideGrid(Tree(verticalIndex, horizontalIndex))
    } yield Tree(verticalIndex, horizontalIndex)

  def isTreeVisibleOutsideGrid(tree: Tree): Boolean = {
    val isVisibleHorizontally = isTreeVisibleInDimension(horizontalTreeMap(tree.y)) _
    val isVisibleVertically = isTreeVisibleInDimension(verticalTreeMap(tree.x)) _
    isVisibleHorizontally(tree.x) || isVisibleVertically(tree.y)
  }

  def isTreeVisibleInDimension(dimensionHeights: Seq[Int])(dimPosition: Int): Boolean = {
    val treesHeightsBefore = dimensionHeights.take(dimPosition)
    val currentHeight = dimensionHeights(dimPosition)
    val treesHeightsAfter = dimensionHeights.drop(dimPosition + 1)

    treesHeightsBefore.forall(_ < currentHeight) || treesHeightsAfter.forall(_ < currentHeight)
  }

  def treesWithScenicScore: Seq[TreeWithScenicScore] =
    for {
      verticalIndex <- verticalTreeMap.keys
      horizontalIndex <- horizontalTreeMap.keys
    }
    yield TreeWithScenicScore(Tree(verticalIndex, horizontalIndex), calculateScenicScore(Tree(verticalIndex, horizontalIndex)))

  def calculateScenicScore(tree: Tree): Int = {
    val (visibleLeftTrees, visibleRightTrees) = countTreesVisibleInDimension(horizontalTreeMap(tree.y))(tree.x)
    val (visibleTopTrees, visibleBottomTrees) = countTreesVisibleInDimension(verticalTreeMap(tree.x))(tree.y)
    Seq(visibleLeftTrees, visibleRightTrees, visibleTopTrees, visibleBottomTrees).product
  }

  def countTreesVisibleInDimension(dimensionHeights: Seq[Int])(dimPosition: Int): (Int, Int) = {
    val treesHeightsBefore = dimensionHeights.take(dimPosition).reverse
    val currentHeight = dimensionHeights(dimPosition)
    val treesHeightsAfter = dimensionHeights.drop(dimPosition + 1)

    (getVisible(currentHeight, treesHeightsBefore), getVisible(currentHeight, treesHeightsAfter))
  }

  def getVisible(currentHeight: Int, treesHeightsAfter: Seq[Int]): Int =
    getVisible(currentHeight, treesHeightsAfter, 0)

  @tailrec
  final def getVisible(currentHeight: Int, treesHeightsAfter: Seq[Int], accumulator: Int): Int =
    treesHeightsAfter.toList match {
      case head :: tail if head < currentHeight => getVisible(currentHeight, tail, accumulator + 1)
      case head :: _ if head >= currentHeight => accumulator + 1
      case _ => accumulator
    }
}

object TreeMap {

  def parseFromFile(filePath: String): TreesMap = {
    val horizontalTreeMap = withSource(filePath)(parseHorizontalTreeMap)
    val verticalTreeMap = swapTreeMapOrientation(horizontalTreeMap)
    TreesMap(verticalTreeMap, horizontalTreeMap)
  }

  private def parseHorizontalTreeMap(source: BufferedSource): ListMap[Int, Seq[Int]] =
    source.getLines()
      .map(_.split("").map(_.toInt).toSeq)
      .zipWithIndex
      .map(_.swap)
      .to(ListMap)

  private def swapTreeMapOrientation(treeMap: ListMap[Int, Seq[Int]]) =
    (0 until getInnerLength(treeMap))
      .map(position => (position, treeMap.map(_._2(position)).toSeq))
      .to(ListMap)

  private def getInnerLength(treeMap: ListMap[Int, Seq[Int]]) =
    treeMap.headOption
      .map(_._2.length)
      .getOrElse(0)
}