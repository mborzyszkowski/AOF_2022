package mb
package supplyStacks

import SourceWrapper._
import supplyStacks.Command.parseCommands
import supplyStacks.CratesStack.parseCratesStacks

object StacksAndCommandsInputParser {

  def parseFromFile(filePath: String): (Seq[CratesStack], Seq[Command]) = {
    val (cratesStacksLines, commandsLines) = withSource(filePath)(source => splitLines(source.getLines().toSeq))
    (parseCratesStacks(cratesStacksLines), parseCommands(commandsLines))
  }

  private def splitLines(lines: Seq[String]): (Seq[String], Seq[String]) = {
    val splitParameter = lines.find(_.isEmpty).get
    val splitParameterIndex = lines.indexOf(splitParameter)
    (lines.slice(0, splitParameterIndex), lines.slice(splitParameterIndex + 1, lines.length))
  }
}
