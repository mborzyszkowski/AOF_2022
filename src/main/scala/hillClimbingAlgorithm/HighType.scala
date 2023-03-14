package mb
package hillClimbingAlgorithm

case class HighType(name: String) {

  def height: Int = elevation - 'a'

  def elevation: Char =
    name match {
      case "S" => 'a'
      case "E" => 'z'
      case _ => name.head
    }
}

object HighType {

  lazy val start: HighType = HighType("S")
  lazy val theBestSignalLocation: HighType = HighType("E")

  def parse(name: String): HighType = HighType(name)
}
