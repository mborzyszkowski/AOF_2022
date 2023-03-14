package mb
package ropeBridge

import SourceWrapper.withSource

import java.lang.Integer.parseInt
import scala.util.Try

sealed trait Move {

  def steps: Int
}

object Move {
  case class Left(steps: Int) extends Move
  case class Up(steps: Int) extends Move
  case class Right(steps: Int) extends Move
  case class Down(steps: Int) extends Move

  def parseFromFile(filePath: String): Seq[Move] =
    withSource(filePath) {
      _.getLines()
        .map(parseMovement)
        .toSeq
    }

  def parseMovement(line: String): Move =
    line.split(" ").toList match {
      case "L" :: steps :: _ if isNumber(steps) => Left(parseInt(steps))
      case "U" :: steps :: _ if isNumber(steps) => Up(parseInt(steps))
      case "R" :: steps :: _ if isNumber(steps) => Right(parseInt(steps))
      case "D" :: steps :: _ if isNumber(steps) => Down(parseInt(steps))
    }

  def isNumber(stringNumber: String): Boolean =
    Try(stringNumber)
      .map(parseInt)
      .isSuccess

  def movePerformed(move: Move): Move =
    move match {
      case Left(steps) => Left(steps - 1)
      case Up(steps) => Up(steps - 1)
      case Right(steps) => Right(steps - 1)
      case Down(steps) => Down(steps - 1)
    }
}
