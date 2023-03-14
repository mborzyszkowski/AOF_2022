package mb
package cathodeRayTube

import java.lang.Integer.parseInt
import scala.util.Try

sealed trait Instruction

object Instruction {
  case class AddX(x: Int) extends Instruction
  case class Noop() extends Instruction
  case class Init() extends Instruction

  def parseFromFile(filePath: String): Seq[Instruction] =
    SourceWrapper.withSource(filePath) {
      _.getLines()
        .map(parseInstruction)
        .toSeq
    }

  def parseInstruction(line: String): Instruction =
    line.split(" ") match {
      case Array("addx", number) if isNumber(number) => AddX(parseInt(number))
      case Array("noop") => Noop()
      case _ => throw new Error(s"Invalid instruction to parse ($line)")
    }

  def isNumber(stringNumber: String): Boolean =
    Try(stringNumber)
      .map(parseInt)
      .isSuccess
}
