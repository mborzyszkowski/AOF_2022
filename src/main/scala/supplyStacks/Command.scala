package mb
package supplyStacks

import supplyStacks.MoveAtOnceCommand.MOVE_COMMAND_NAME

trait Command {

  def execute(list: Seq[CratesStack]): Unit
}

object Command {

  def parseCommands(lines: Seq[String]): Seq[Command] =
    lines.map(parseCommand)

  def parseCommand(line: String): Command = {
    val commandName = line.split(" ")(0)

    commandName match {
      case MOVE_COMMAND_NAME => MoveByOneCommand.parse(line)
      case _ => throw new NotImplementedError()
    }
  }
}