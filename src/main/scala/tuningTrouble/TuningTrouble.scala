package mb
package tuningTrouble

import SourceWrapper.withSource

object TuningTrouble extends App {

  val filePath = "src/main/scala/tuningTrouble/input.txt"

  val firstMarker = {
    val windowSize = 4
    withSource(filePath) {
      source =>
        StreamWindowIterator(source.iterator, windowSize)
          .find(_.content.distinct.length == windowSize)
          .get
    }
  }

  println(s"W4: Start of packet marker ${firstMarker.content} after ${firstMarker.lastContentElementPosition} elements of data stream")

  val secondMarker = {
    val windowSize = 14
    withSource(filePath) {
      source =>
        StreamWindowIterator(source.iterator, windowSize)
          .find(_.content.distinct.length == windowSize)
          .get
    }
  }

  println(s"W14: Start of packet marker ${secondMarker.content} after ${secondMarker.lastContentElementPosition} elements of data stream")
}
