package mb
package monkeyInTheMiddle

case class Item(initialWorryLevel: BigInt, worryLevel: BigInt) {

  def changeWorryLevel(newWorryLevel: BigInt): Item = Item(initialWorryLevel, newWorryLevel)
}

object Item {

  def apply(initialWorryLevel: BigInt): Item = Item(initialWorryLevel, initialWorryLevel)
}
