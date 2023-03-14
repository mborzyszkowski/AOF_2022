package mb
package treetopTreeHouse

object TreetopTreeHouse extends App {

  val filePath = "src/main/scala/treetopTreeHouse/input.txt"

  val treeMap = TreeMap.parseFromFile(filePath)

  val visibleTreesOutsideGrid = treeMap.visibleTrees

  println(s"Visible trees(${visibleTreesOutsideGrid.length}): ${visibleTreesOutsideGrid.mkString(", ")}")

  val treesWithScenicScore =
    treeMap.treesWithScenicScore
      .sortBy(_.scenicScore)
      .reverse

  val treeWithHighestScenicScore = treesWithScenicScore.head

  println(s"Tree with highest scenic score (${treeWithHighestScenicScore.scenicScore}) is ${treeWithHighestScenicScore.tree}")
}
