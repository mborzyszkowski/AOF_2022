package mb
package rockPaperScissors

import SourceWrapper.withSource

trait Strategy {

  def calculateScore(): Int
}

object Strategy {

  def parseFromFile(filePath: String)(parseSymbols: Array[String] => Option[Strategy]): Seq[Strategy] =
    withSource(filePath) {
      _.getLines()
        .map(_.split(" "))
        .map(parseSymbols)
        .filter(_.nonEmpty)
        .map(_.get)
        .toSeq
    }
}
