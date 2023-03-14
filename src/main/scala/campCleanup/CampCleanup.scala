package mb
package campCleanup

object CampCleanup extends App {

  val filePath = "src/main/scala/campCleanup/input.txt"


  val assignmentsPairs = AssignmentsPair.parseFromFile(filePath)


  val assignmentsPairsThatOverlapsFullyCount = assignmentsPairs.count(_.oneAssignmentOverlapsFullyOther)

  println(s"Assignments pairs that overlaps fully count: $assignmentsPairsThatOverlapsFullyCount")


  val assignmentsPairsThatOverlapsPartiallyCount = assignmentsPairs.count(_.oneAssignmentOverlapsPartiallyOther)

  println(s"Assignments pairs that overlaps partially count: $assignmentsPairsThatOverlapsPartiallyCount")
}
