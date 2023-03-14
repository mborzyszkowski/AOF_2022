package mb
package campCleanup

import java.lang.Integer.parseInt

case class Assignment(start: Int, end: Int) {

  def overlapsFully(otherAssignment: Assignment): Boolean =
    this.start <= otherAssignment.start && this.end >= otherAssignment.end

  def overlapsPartially(otherAssignment: Assignment): Boolean =
    this.start <= otherAssignment.start && this.end >= otherAssignment.start
}

object Assignment {

  def parse(line: String): Assignment = {
    val ranges = line.split("-")
    Assignment(parseInt(ranges.head), parseInt(ranges.tail.head))
  }
}