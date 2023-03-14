package mb
package supplyStacks

case class InvalidCommand(invalidLine: String) extends Command {

  override def execute(list: Seq[CratesStack]): Unit = {}
}
