package mb
package supplyStacks

import java.lang.Integer.parseInt
import scala.util.Try

case class MoveByOneCommand(numberOfCrates: Int, sourceStackName: String, destinationStackName: String) extends Command {

  override def execute(cratesStacks: Seq[CratesStack]): Unit = {
    val sourceStack = cratesStacks.find(_.name == sourceStackName).get.stack
    val destinationStack = cratesStacks.find(_.name == destinationStackName).get.stack

    (1 to numberOfCrates).foreach(_ => destinationStack.push(sourceStack.pop()))
  }
}

object MoveByOneCommand {

  def parse(line: String): Command = {
    val lineParts = line.split(" ")

    val command =
      for {
        numberOfCrates <- Try(parseInt(lineParts(1))).toOption
        sourceStackName <- Try(lineParts(3)).toOption
        destinationStackName <- Try(lineParts(5)).toOption
      } yield MoveByOneCommand(numberOfCrates, sourceStackName, destinationStackName)

    command.getOrElse(InvalidCommand(line))
  }
}
