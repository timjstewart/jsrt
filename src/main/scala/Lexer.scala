sealed case class Position(line: Int, column: Int) {
  def moveByChar(char: Char): Position = char match {
    case '\n' => this.copy(line = line + 1, column = 0)
    case '\r' => this.copy(column = 0)
    case _    => this.copy(column = column + 1)
  }
}

sealed abstract class Token(value: String, pos: Position)
case class NEWLINE(pos: Position) extends Token("\n", pos)
case class QUOTE(pos: Position) extends Token("\"", pos)
case class COMMA(pos: Position) extends Token(",", pos)
case class OPEN_SQUARE_BRACKET(pos: Position) extends Token("[", pos)
case class CLOSE_SQUARE_BRACKET(pos: Position) extends Token("]", pos)
case class OPEN_CURLY_BRACE(pos: Position) extends Token("{", pos)
case class CLOSE_CURLY_BRACE(pos: Position) extends Token("}", pos)
case class COLON(pos: Position) extends Token(":", pos)
case class CHARACTER(value: Character, pos: Position)
    extends Token(value.toString, pos)
case class STRING(value: String, pos: Position) extends Token(value, pos)

case class EOF(pos: Position) extends Token("EOF", pos)

object Lexer {

  import scala.io.Source

  def tokenize(input: Source): Either[Exception, List[Token]] = {
    var curPos = Position(1, 0)
    Right(input.map { c =>
      c match {
        case '\n' =>
          val newLinePos = curPos.copy(column = curPos.column + 1)
          curPos = curPos.moveByChar(c)
          NEWLINE(newLinePos)
        case _ =>
          curPos = curPos.moveByChar(c)
          c match {
            // case '"' => {
            //   val origPos = curPos.copy()
            //   val (string,newPos) = tokenizeString(input, origPos)
            //   curPos = newPos
            //   STRING(string, origPos)
            // }
            case ',' => COMMA(curPos)
            case '[' => OPEN_SQUARE_BRACKET(curPos)
            case ']' => CLOSE_SQUARE_BRACKET(curPos)
            case '{' => OPEN_CURLY_BRACE(curPos)
            case '}' => CLOSE_CURLY_BRACE(curPos)
            case ':' => COLON(curPos)
            case _   => CHARACTER(c, curPos)
          }
      }
    }.toList :+ EOF(curPos.copy(column = curPos.column + 1)))
  }

  // private def tokenizeString(input: Iterator[Char], startPos: Position): Tuple2[String, Position] = {
  //   val result = new StringBuffer()
  //   var escaped = false

  //   do {
  //     val c = input.next()

  //   } while(false)

  //   ("", startPos.copy(column = startPos.column + 1))
  // }
}
