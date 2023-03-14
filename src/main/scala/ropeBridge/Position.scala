package mb
package ropeBridge

import scala.math.{max, min}

case class Position(x: Int, y: Int) {

  def move(vx: Int, vy: Int): Position =
    Position(x + vx, y + vy)

  def moveToDest(destPosition: Position): Position =
    move(maxMove(destPosition.x - this.x), maxMove(destPosition.y - this.y))

  def maxMove(dimMove: Int): Int =
    max(-1, min(1, dimMove))
}
