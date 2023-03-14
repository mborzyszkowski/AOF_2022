package mb
package supplyStacks

import scala.collection.mutable

case class CratesStack(name: String, stack: mutable.Stack[Crate])

object CratesStack {

  def parseCratesStacks(lines: Seq[String]): Seq[CratesStack] = {
    val cratesAndPositionsLines =
      lines.map(_.grouped(4).toSeq)
        .map(_.map(_.trim))

    val cratesLines = cratesAndPositionsLines.take(lines.indexOf(lines.last))
    val stacksNames = cratesAndPositionsLines.last

    stacksNames.zipWithIndex.map { case (name, index) => toCratesStack(cratesLines, name, index) }
  }

  private def toCratesStack(cratesLines: Seq[Seq[String]], name: String, index: Int) =
    CratesStack(name, mutable.Stack().pushAll(collectCratesForStack(cratesLines, index)))

  private def collectCratesForStack(cratesLines: Seq[Seq[String]], index: Int): Seq[Crate] =
    cratesLines.map(_.lift(index))
      .filter(_.nonEmpty)
      .map(_.get)
      .filterNot(_.isEmpty)
      .map(Crate)
      .reverse
}