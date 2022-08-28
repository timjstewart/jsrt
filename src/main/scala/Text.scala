package text

object Text {
  implicit class RichText(s: String) {
    def unindent(): String = {
      val level = s
        .split('\n')
        .map { line =>
          val result = line.indexWhere(c => c != ' ') match {
            case -1     => 0
            case x: Int => x
          }
          result
        }
        .min
      if (level > 0) {
        s.split('\n')
          .map { line =>
            line.substring(level)
          }
          .mkString("\n")
      } else {
        s
      }
    }
  }
}
