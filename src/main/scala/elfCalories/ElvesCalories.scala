package mb
package elfCalories

object ElvesCalories extends App {

  val filePath = "src/main/scala/elfCalories/input.txt"
  val topElvesNumber = 3

  val elves = Elf.parseFromFile(filePath)

  println(s"\nMax calories: ${elves.maxBy(_.calories).calories}\n")

  val topMaxElvesCalories =
    elves.sortBy(_.calories)
      .reverse
      .take(topElvesNumber)

  topMaxElvesCalories.foreach(elf => println(s"Calories for elf ${elf.name} is ${elf.calories}"))

  println(s"\nSum of calories of top $topElvesNumber elves is ${topMaxElvesCalories.map(_.calories).sum}\n")
}
