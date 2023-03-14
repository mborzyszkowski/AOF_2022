package mb
package supplyStacks

case class MoveAtOnceCommand(numberOfCrates: Int, sourceStackName: String, destinationStackName: String) extends Command {

  override def execute(cratesStacks: Seq[CratesStack]): Unit = {
    val sourceStack = cratesStacks.find(_.name == sourceStackName).get.stack
    val destinationStack = cratesStacks.find(_.name == destinationStackName).get.stack

    (1 to numberOfCrates)
      .map(_ => sourceStack.pop())
      .reverse
      .foreach(destinationStack.push)
  }
}

object MoveAtOnceCommand {

  val MOVE_COMMAND_NAME = "move"

  def toMoveAtOnceCommand(moveByOneCommand: MoveByOneCommand): MoveAtOnceCommand =
    MoveAtOnceCommand(moveByOneCommand.numberOfCrates, moveByOneCommand.sourceStackName, moveByOneCommand.destinationStackName)
}
