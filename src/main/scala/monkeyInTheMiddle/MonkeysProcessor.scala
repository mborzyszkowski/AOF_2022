package mb
package monkeyInTheMiddle

import scala.annotation.tailrec

object MonkeysProcessor {

  def processTurns(monkeys: Seq[Monkey], turns: Int): (Seq[Monkey], Map[MonkeyReference, Long]) =
    processTurnsRec(monkeys, turns, monkeys.map(monkey => (monkey.monkeyReference, 0L)).toMap)

  @tailrec
  final def processTurnsRec(monkeys: Seq[Monkey], turns: Int, monkeysInspectTimes: Map[MonkeyReference, Long]): (Seq[Monkey], Map[MonkeyReference, Long]) =
    turns match {
      case 0 => (monkeys, monkeysInspectTimes)
      case _ =>
        val (newMonkeys, newMonkeysInspectTimes) = processOneTurn(monkeys, monkeysInspectTimes)
        val monkeysModuloProduct = newMonkeys.map(_.modulo).product
        processTurnsRec(newMonkeys.map(_.withReduceItemsSizes(monkeysModuloProduct)), turns - 1, newMonkeysInspectTimes)
    }

  def processOneTurn(monkeys: Seq[Monkey], monkeysInspectTimes: Map[MonkeyReference, Long]): (Seq[Monkey], Map[MonkeyReference, Long]) =
    processOneTurnRec(monkeys, monkeysInspectTimes, Seq())

  @tailrec
  final def processOneTurnRec(monkeys: Seq[Monkey], monkeysInspectTimes: Map[MonkeyReference, Long], newMonkeys: Seq[Monkey]): (Seq[Monkey], Map[MonkeyReference, Long]) =
    monkeys match {
      case monkey :: restMonkeys =>
        val itemsForMonkeys = monkey.processTurn
        val newMonkeysInspectTimes = monkeysInspectTimes.updated(monkey.monkeyReference, monkeysInspectTimes(monkey.monkeyReference) + monkey.items.size)
        val processedMonkeys =
          (newMonkeys :+ monkey.withNewItems(Seq()))
            .map(newMonkey => newMonkey.withNewItems(newMonkey.items ++ newMonkey.getItemsForMonkey(itemsForMonkeys)))
        val monkeysToProcess = restMonkeys.map(otherMonkey => otherMonkey.withNewItems(otherMonkey.items ++ otherMonkey.getItemsForMonkey(itemsForMonkeys)))
        processOneTurnRec(monkeysToProcess, newMonkeysInspectTimes, processedMonkeys)
      case _ =>
        (newMonkeys, monkeysInspectTimes)
    }
}
