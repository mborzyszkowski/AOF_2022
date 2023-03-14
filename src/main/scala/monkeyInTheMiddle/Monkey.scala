package mb
package monkeyInTheMiddle

import SourceWrapper.withSource

import java.lang.Integer.parseInt
import scala.util.Try

case class Monkey(name: String, itemTransformer: Item => Item, throwTest: Item => MonkeyReference, items: Seq[Item], worryLevelDivideNumber: Int, modulo: Int) {

  def monkeyReference: MonkeyReference = MonkeyReference(name)

  def processTurn: Seq[(MonkeyReference, Item)] =
    items.map(item => itemTransformer.apply(item))
      .map(item => item.changeWorryLevel(item.worryLevel / worryLevelDivideNumber))
      .map(item => (throwTest.apply(item), item))

  def withNewItems(newItems: Seq[Item]): Monkey =
    Monkey(name, itemTransformer, throwTest, newItems, worryLevelDivideNumber, modulo)

  def getItemsForMonkey(itemsForMonkeys: Seq[(MonkeyReference, Item)]): Seq[Item] =
    itemsForMonkeys.filter(_._1 == monkeyReference)
      .map(_._2)

  def withReduceItemsSizes(productsOfAllModulo: BigInt): Monkey =
    withNewItems(items.map(item => item.changeWorryLevel(item.worryLevel % productsOfAllModulo)))
}

object Monkey {

  sealed trait MonkeyProperty
  case class Name(name: String) extends MonkeyProperty
  case class InitialItems(items: Seq[Item]) extends MonkeyProperty
  case class Operation(itemTransformer: Item => Item) extends MonkeyProperty
  case class ThrowTest(throwTest: Item => Boolean, testNumber: Int) extends MonkeyProperty
  case class OnPositiveThrowTest(monkeyReference: MonkeyReference) extends MonkeyProperty
  case class OnNegativeThrowTest(monkeyReference: MonkeyReference) extends MonkeyProperty

  def apply(name: Name, operation: Operation, throwTest: ThrowTest, onPositiveThrowTest: OnPositiveThrowTest,
            onNegativeThrowTest: OnNegativeThrowTest, initialItems: InitialItems, worryLevelDivideNumber: Int): Monkey =
    Monkey(name.name,
      operation.itemTransformer,
      item => createThrowTest(throwTest, onPositiveThrowTest, onNegativeThrowTest)(item),
      initialItems.items,
      worryLevelDivideNumber,
      throwTest.testNumber)

  private def createThrowTest(throwTest: ThrowTest, onPositiveThrowTest: OnPositiveThrowTest, onNegativeThrowTest: OnNegativeThrowTest)(item: Item): MonkeyReference =
    if (throwTest.throwTest(item))
      onPositiveThrowTest.monkeyReference
    else
      onNegativeThrowTest.monkeyReference

  def parseFromFile(filePath: String, worryLevelDivideNumber: Int): Seq[Monkey] =
    withSource(filePath) {
      _.getLines()
        .map(_.trim)
        .filter(_.nonEmpty)
        .grouped(6)
        .map(parseMonkey(_, worryLevelDivideNumber))
        .toSeq
    }

  def parseMonkey(lines: Seq[String], worryLevelDivideNumber: Int): Monkey = {
    val monkeyProperties = lines.map(parseMonkeyAttribute)
    Monkey.createFromProperties(monkeyProperties, worryLevelDivideNumber).get
  }

  def parseMonkeyAttribute(line: String): MonkeyProperty =
    line.split(" ").toList match {
      case "Monkey" :: name :: _ =>
        Name(name.filter(_ != ':'))
      case "Starting" :: "items:" :: items if areItemsParsable(items) =>
        InitialItems(items.map(_.filter(_ != ',')).map(number => Item(parseInt(number))))
      case "Operation:" :: "new" :: "=" :: "old" :: operator :: "old" :: _ if isOperator(operator) =>
        Operation(item => item.changeWorryLevel(calculateOperation(item.worryLevel, operator, item.worryLevel)))
      case "Operation:" :: "new" :: "=" :: "old" :: operator :: number :: _ if isOperator(operator) && isNumber(number) =>
        Operation(item => item.changeWorryLevel(calculateOperation(item.worryLevel, operator, parseInt(number))))
      case "Test:" :: "divisible" :: "by" :: number :: _ if isNumber(number) =>
        ThrowTest(item => item.worryLevel % parseInt(number) == 0, parseInt(number))
      case "If" :: "true:" :: "throw" :: "to" :: "monkey" :: monkeyName :: _ =>
        OnPositiveThrowTest(MonkeyReference(monkeyName))
      case "If" :: "false:" :: "throw" :: "to" :: "monkey" :: monkeyName :: _ =>
        OnNegativeThrowTest(MonkeyReference(monkeyName))
      case _ => throw new Error(s"Can not parse Monkey property $line")
    }

  def calculateOperation(worryLevel: BigInt, operator: String, otherWorryLevel: BigInt): BigInt =
    operator match {
      case "*" => worryLevel * otherWorryLevel
      case "+" => worryLevel + otherWorryLevel
      case "-" => worryLevel - otherWorryLevel
      case _ => throw new Error(s"Operator '$operator' is not defined'")
    }

  def areItemsParsable(items: Seq[String]): Boolean =
    Try(items)
      .map(_.map(_.filter(_ != ',')))
      .map(_.map(parseInt))
      .isSuccess

  def isOperator(operator: String): Boolean = "*+".contains(operator)

  def isNumber(number: String): Boolean = Try(parseInt(number)).isSuccess

  def createFromProperties(monkeyProperties: Seq[MonkeyProperty], worryLevelDivideNumber: Int): Option[Monkey] =
    for {
      name <- monkeyProperties.find(_.isInstanceOf[Name])
      operation <- monkeyProperties.find(_.isInstanceOf[Operation])
      throwTest <- monkeyProperties.find(_.isInstanceOf[ThrowTest])
      onPositiveThrowTest <- monkeyProperties.find(_.isInstanceOf[OnPositiveThrowTest])
      onNegativeThrowTest <- monkeyProperties.find(_.isInstanceOf[OnNegativeThrowTest])
      initialItems <- monkeyProperties.find(_.isInstanceOf[InitialItems])
    } yield
      (name, operation, throwTest, onPositiveThrowTest, onNegativeThrowTest, initialItems) match {
        case (name: Name, operation: Operation, throwTest: ThrowTest, onPositiveThrowTest: OnPositiveThrowTest, onNegativeThrowTest: OnNegativeThrowTest, initialItems: InitialItems) =>
          Monkey(name, operation, throwTest, onPositiveThrowTest, onNegativeThrowTest, initialItems, worryLevelDivideNumber)
      }
}