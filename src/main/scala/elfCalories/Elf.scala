package mb
package elfCalories

import SourceWrapper.withSource

import java.lang.Integer.parseInt

case class Elf(name: String, calories: Int)

object Elf {

  def parseFromFile(filePath: String): Seq[Elf] =
    withSource(filePath) {
      _.getLines()
        .map(line => if (line == "") List() else List(parseInt(line)))
        .fold(List(0)) { case (left, right) => if (right.isEmpty) left :+ 0 else left.tail :+ (left.head + right.head) }
        .zipWithIndex
        .map { case (calories, index) => Elf(s"$index", calories) }
    }
}
