package mb
package monkeyInTheMiddle

object MonkeyInTheMiddle extends App {

  val filePath = "src/main/scala/monkeyInTheMiddle/input.txt"

  {
    val monkeys = Monkey.parseFromFile(filePath, 3)

    val (monkeysAfterTwentyTurns, monkeysToItemInspectsAfterTwentyTurns) = MonkeysProcessor.processTurns(monkeys, 20)

    monkeysAfterTwentyTurns.foreach(println)
    println(monkeysToItemInspectsAfterTwentyTurns)

    val monkeyBusiness =
      monkeysToItemInspectsAfterTwentyTurns.toSeq
        .map(_._2)
        .sorted
        .reverse
        .take(2)
        .product

    println(s"monkey business: $monkeyBusiness")
  }

  {
    val monkeys = Monkey.parseFromFile(filePath, 1)

    val (monkeysAfterTwentyTurns, monkeysToItemInspectsAfterTwentyTurns) = MonkeysProcessor.processTurns(monkeys, 10000)

    monkeysAfterTwentyTurns.foreach(println)
    println(monkeysToItemInspectsAfterTwentyTurns)

    val monkeyBusiness =
      monkeysToItemInspectsAfterTwentyTurns.toSeq
        .map(_._2)
        .sorted
        .reverse
        .take(2)
        .product

    println(s"monkey business: $monkeyBusiness")
  }
}