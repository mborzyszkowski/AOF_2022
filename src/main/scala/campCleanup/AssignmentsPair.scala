package mb
package campCleanup

import SourceWrapper.withSource

case class AssignmentsPair(firstAssignment: Assignment, secondAssignment: Assignment) {

  def oneAssignmentOverlapsFullyOther: Boolean =
    firstAssignment.overlapsFully(secondAssignment) || secondAssignment.overlapsFully(firstAssignment)

  def oneAssignmentOverlapsPartiallyOther: Boolean =
    firstAssignment.overlapsPartially(secondAssignment) || secondAssignment.overlapsPartially(firstAssignment)
}

object AssignmentsPair {

  def parseFromFile(filePath: String): Seq[AssignmentsPair] =
    withSource(filePath) {
      _.getLines()
        .map(_.split(","))
        .map(rawAssignments => AssignmentsPair(Assignment.parse(rawAssignments.head), Assignment.parse(rawAssignments.tail.head)))
        .toSeq
    }
}