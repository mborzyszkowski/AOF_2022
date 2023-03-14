package mb

import scala.io.{BufferedSource, Source}

object SourceWrapper {

  def withSource[TResult](filePath: String)(code: BufferedSource => TResult): TResult = {
    val source = Source.fromFile(filePath)
    val result = code.apply(source)
    source.close()
    result
  }
}
