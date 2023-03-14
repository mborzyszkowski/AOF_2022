package mb
package supplyStacks

import supplyStacks.MoveAtOnceCommand.toMoveAtOnceCommand

object SupplyStacks extends App {

  val filePath = "src/main/scala/supplyStacks/input.txt"


  val (cratesStacks, commands) = StacksAndCommandsInputParser.parseFromFile(filePath)

  commands.foreach(_.execute(cratesStacks))

  val highestStack = cratesStacks.maxBy(_.stack.length)
  val topStacksCrates = cratesStacks.map(_.stack.head.name.charAt(1)).mkString

  println()
  println(s"Highest crates stack after commands execution is stack ${highestStack.name} " +
    s"with height ${highestStack.stack.length} and element on top ${highestStack.stack.head.name}")
  println(s"Crates on top ${topStacksCrates}")


  val (secondCratesStacks, secondCommands) = StacksAndCommandsInputParser.parseFromFile(filePath)

  secondCommands
    .filter(_.isInstanceOf[MoveByOneCommand])
    .map(_.asInstanceOf[MoveByOneCommand])
    .map(toMoveAtOnceCommand)
    .foreach(_.execute(secondCratesStacks))

  val topSecondStacksCrates = secondCratesStacks.map(_.stack.head.name.charAt(1)).mkString

  println()
  println(s"Crates on top on second stacks group ${topSecondStacksCrates}")
}
