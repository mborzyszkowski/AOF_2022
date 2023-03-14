package mb
package ropeBridge

import ropeBridge.Move.{Down, Left, Right, Up, movePerformed}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.math.abs

case class Climber(headPosition: Position, knots: Int, tail: Queue[Position]) {

  def makeMoves(moves: Seq[Move]): (Climber, Set[Position]) =
    makeMoves(this, moves.toList, Set(Position(0, 0)))

  @tailrec
  final def makeMoves(climber: Climber, moves: List[Move], visitedPositionsByTail: Set[Position]): (Climber, Set[Position]) =
    moves match {
      case move :: otherMoves if move.steps > 1 =>
        val movedClimber = climber.makeMove(move)
        makeMoves(movedClimber, movePerformed(move) :: otherMoves, visitedPositionsByTail + movedClimber.tail.last)
      case move :: otherMoves if move.steps == 1 =>
        val movedClimber = climber.makeMove(move)
        makeMoves(movedClimber, otherMoves, visitedPositionsByTail + movedClimber.tail.last)
      case _ => (climber, visitedPositionsByTail)
    }

  def makeMove(move: Move): Climber =
    move match {
      case Left(_) => changePosition(-1, 0)
      case Up(_) => changePosition(0, 1)
      case Right(_) => changePosition(1, 0)
      case Down(_) => changePosition(0, -1)
    }

  def changePosition(vx: Int, vy: Int): Climber = {
    val newHeadPosition = this.headPosition.move(vx, vy)
    val newTailPosition = moveRope(newHeadPosition, this.tail, Queue())

    Climber(newHeadPosition, this.knots, newTailPosition)
  }

  @tailrec
  final def moveRope(previousKnotPosition: Position, restKnots: Queue[Position], newKnotsPositions: Queue[Position]): Queue[Position] =
    restKnots match {
      case knotPosition +: restKnots =>
        if (isKnotFurtherAwayThenOtherKnot(previousKnotPosition, knotPosition)) {
          val movedKnot = knotPosition.moveToDest(previousKnotPosition)
          moveRope(movedKnot, restKnots, newKnotsPositions :+ movedKnot)
        } else
          moveRope(knotPosition, restKnots, newKnotsPositions :+ knotPosition)
      case _ => newKnotsPositions
    }

  private def isKnotFurtherAwayThenOtherKnot(knot: Position, otherKnot: Position) =
    abs(knot.x - otherKnot.x) > 1 || abs(knot.y - otherKnot.y) > 1
}

object Climber {

  def apply(): Climber = apply(1)

  def apply(knots: Int): Climber = Climber(Position(0, 0), knots, Queue((0 until knots).map(_ => Position(0, 0)): _*))
}