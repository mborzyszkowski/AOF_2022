package mb
package tuningTrouble

case class StreamWindowIterator(inputIterator: Iterator[Char], windowWidth: Int) extends Iterator[WindowElement] {

  private var currentWindowContent = inputIterator.take(windowWidth).mkString
  private var currentPosition = 0

  override def hasNext: Boolean =
    inputIterator.hasNext

  override def next(): WindowElement = {
    val result = WindowElement(currentWindowContent, windowWidth + currentPosition)
    currentWindowContent = currentWindowContent.drop(1) + (if (inputIterator.hasNext) inputIterator.next() else "")
    currentPosition += 1
    result
  }
}
