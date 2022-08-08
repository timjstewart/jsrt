object Lexer {

  import scala.io.Source

  type Result = Either[String, Tuple2[List[Token], Buffer]]

  sealed case class Buffer(
      text: String,
      line: Int = 1,
      column: Int = 1,
      index: Int = 0
  ) {
    val atEof = index + 1 > text.length
    val currentChar = if (atEof) None else Some(text(index))

    def advance(): Buffer = {
      println("ADVANCE")
      if (index + 1 == text.length) {
        this.copy(column = column + 1, index = index + 1)
      } else if (index + 1 > text.length) {
        this
      } else {
        currentChar match {
          case Some('\n') =>
            this.copy(line = line + 1, column = 1, index = index + 1)
          case Some('\r') => this.copy(column = 1, index = index + 1)
          case Some(c) =>
            println("OTHER: %s".format(c))
            this.copy(column = column + 1, index = index + 1)
          case None => this
        }
      }
    }

    def scan(f: Buffer => Result, accum: List[Token] = Nil): Result = {
      scanWhile(b => !b.atEof)(f)
    }

    def scanWhile(
        cond: Buffer => Boolean
    )(f: Buffer => Result, accum: List[Token] = Nil): Result = {
      println("SCAN")
      if (!cond(this)) {
        println("isEOF")
        Right(Tuple2(accum, this))
      } else {
        f(this) match {
          case Left(error) =>
            println("ERROR: %s".format(error))
            Left(error)
          case Right(Tuple2(tokens, remaining)) if !atEof =>
            println("RIGHT !isEOF: %s, %s".format(tokens, remaining))
            remaining.scanWhile(cond)(f, accum ::: tokens)
          case Right(Tuple2(tokens, remaining)) =>
            println("RIGHT")
            Right(Tuple2(tokens, remaining))
        }
      }
    }
  }

  sealed abstract class Token(value: String, pos: Buffer)
  case class NewLineToken(pos: Buffer) extends Token("\n", pos)
  case class QuoteToken(pos: Buffer) extends Token("\"", pos)
  case class CommaToken(pos: Buffer) extends Token(",", pos)
  case class OpenSquareBracketToken(pos: Buffer) extends Token("[", pos)
  case class CloseSquareBracketToken(pos: Buffer) extends Token("]", pos)
  case class OpenCurlyBraceToken(pos: Buffer) extends Token("{", pos)
  case class CloseCurlyBraceToken(pos: Buffer) extends Token("}", pos)
  case class ColonToken(pos: Buffer) extends Token(":", pos)
  case class CharacterToken(value: Character, pos: Buffer)
      extends Token(value.toString, pos)
  case class StringToken(value: String, pos: Buffer) extends Token(value, pos)
  case class BoolTrueToken(pos: Buffer) extends Token("true", pos)
  case class BoolFalseToken(pos: Buffer) extends Token("false", pos)
  case class NumberToken(value: Double, pos: Buffer)
      extends Token(value.toString, pos)

  case class EOF(pos: Buffer) extends Token("EOF", pos)

  class LexerError(message: String) extends Exception(message)

  def success(buffer: Buffer, tokens: Token*): Result = {
    Right(Tuple2(tokens.toList, buffer))
  }

  def tokenize(input: String): Result = {
    println("TOKENIZE! %s".format(input))
    var buffer = Buffer(input)
    buffer
      .scan { b =>
        println("SCANNED: %s".format(b))
        b.currentChar match {
          case Some(',')  => success(b.advance(), CommaToken(b))
          case Some('[')  => success(b.advance(), OpenSquareBracketToken(b))
          case Some(']')  => success(b.advance(), CloseSquareBracketToken(b))
          case Some('{')  => success(b.advance(), OpenCurlyBraceToken(b))
          case Some('}')  => success(b.advance(), CloseCurlyBraceToken(b))
          case Some(':')  => success(b.advance(), ColonToken(b))
          case Some('\n') => success(b.advance(), NewLineToken(b))
          case Some('"') =>
            val x = tokenizeString(b)
            println("TOK STR: %s".format(x))
            x
          case c => Left("lexer fail: %s".format(c))
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        println("HIT EOF 1")
        Tuple2(tokens :+ EOF(remaining), remaining)
      }
  }

  private def tokenizeString(startPos: Buffer): Result = {
    val str = new StringBuffer("\"")
    var escaped = false
    var done = false
// pass the first double quote
    startPos
      .advance()
      .scanWhile(_ => !done) { buffer =>
        buffer.currentChar match {
          case Some('\\') =>
            escaped = true
            str.append('\\')
            success(buffer.advance())
          case Some('"') if escaped =>
            str.append('"')
            escaped = false
            success(buffer.advance())
          case Some('"') =>
            println("END OF STRING: %s".format(str.toString))
            done = true
            str.append('"')
            success(buffer.advance())
          case Some(c) =>
            println("LETTER: %s".format(c))
            str.append(c)
            success(buffer.advance())
          case None =>
            println("NONE CHAR")
            str.append('"')
            done = true
            success(buffer.advance())
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        println("STR 1: %s".format(str.toString))
        Tuple2(
          tokens :+ StringToken(str.toString, startPos),
          remaining.advance()
        )
      }
  }
  //   Right(input.flatMap { c =>
  //     c match {
  //       case '\n' =>
  //         val newLinePos = curPos.copy(column = curPos.column + 1)
  //         curPos = curPos.advance()
  //         List(NEWLINE(newLinePos))
  //       case _ =>
  //         curPos = curPos.advance()
  //         c match {
  //           case '"' =>
  //             val origPos = curPos.copy()
  //             val (string, newPos) = tokenizeString(input, curPos)
  //             curPos = newPos
  //             List(STRING(string, origPos))
  //           case ',' => List(COMMA(curPos))
  //           case '[' => List(OPEN_SQUARE_BRACKET(curPos))
  //           case ']' => List(CLOSE_SQUARE_BRACKET(curPos))
  //           case '{' => List(OPEN_CURLY_BRACE(curPos))
  //           case '}' => List(CLOSE_CURLY_BRACE(curPos))
  //           case ':' => List(COLON(curPos))
  //           case 't' =>
  //             val origPos = curPos.copy()
  //             tokenizeBool(input, c, curPos, "true") match {
  //               case Right(Tuple2(token, newPos)) =>
  //                 curPos = newPos
  //                 List(token)
  //               case Left(error) => throw new LexerError(error)
  //             }
  //           case 'f' =>
  //             val origPos = curPos.copy()
  //             tokenizeBool(input, c, curPos, "false") match {
  //               case Right(Tuple2(token, newPos)) =>
  //                 curPos = newPos
  //                 List(token)
  //               case Left(error) => throw new LexerError(error)
  //             }
  //           case digit if digit.isDigit =>
  //             val origPos = curPos.copy()
  //             tokenizeNumber(input, c, curPos) match {
  //               case Right(Tuple2(tokens, newPos)) =>
  //                 curPos = newPos
  //                 tokens
  //               case Left(error) => throw new LexerError(error)
  //             }
  //           case _ => List(CHARACTER(c, curPos))
  //         }
  //     }
  //   }.toList :+ EOF(curPos.copy(column = curPos.column + 1)))
  // } catch {
  //   case ex: LexerError => Left(ex.getMessage)
  // }

  // private def tokenizeString(
  //     input: Iterator[Char],
  //     startPos: Buffer
  // ): Tuple2[String, Buffer] = {
  //   val result = new StringBuffer()
  //   var curPos = startPos.copy()
  //   var done = false
  //   var escaped = false
  //   do {
  //     if (input.hasNext) {
  //       val c = input.next()
  //       curPos = curPos.advance()

  //       if (c == '"' && !escaped) {
  //         done = true
  //       } else {
  //         result.append(c)
  //       }

  //       escaped = false

  //       if (c == '\\') {
  //         escaped = true
  //       }

  //     } else {
  //       done = true
  //     }
  //   } while (input.hasNext && !done)
  //   (result.toString, curPos)
  // }

  // private def tokenizeBool(
  //     input: Iterator[Char],
  //     firstChar: Char,
  //     startPos: Buffer,
  //     literal: String
  // ): Either[String, Tuple2[Token, Buffer]] = {
  //   require(firstChar == literal(0))
  //   var pos = startPos.copy()
  //   try {
  //     for (char <- literal.drop(1)) {
  //       val readChar = input.next()
  //       pos = pos.advance()
  //       if (readChar != char) {
  //         throw new Exception(
  //           "mismatch: expected: %s, read: %s".format(char, readChar)
  //         )
  //       }
  //     }
  //     literal match {
  //       case "true"  => Right(BOOL_TRUE(startPos), pos)
  //       case "false" => Right(BOOL_FALSE(startPos), pos)
  //       case _       => Left("failed to tokenize boolean")
  //     }
  //   } catch {
  //     case ex: Exception =>
  //       println("EX: %s".format(ex.getMessage()))
  //       Left(ex.getMessage())
  //   }
  // }

  // private def tokenizeNumber(
  //     input: Iterator[Char],
  //     firstChar: Char,
  //     startPos: Buffer
  // ): Either[String, Tuple2[List[Token], Buffer]] = {
  //   require(firstChar.isDigit)
  //   var pos = startPos.copy()
  //   Right(List(NUMBER(123.0, startPos)), startPos)
  //   // try {
  //   //     val readChar = input.next()
  //   //     pos = pos.advance()
  //   //     if (readChar != char) {
  //   //       throw new Exception(
  //   //         "mismatch: expected: %s, read: %s".format(char, readChar)
  //   //       )
  //   //     }
  //   // } catch {
  //   //   case ex: Exception =>
  //   //     println("EX: %s".format(ex.getMessage()))
  //   //     Left(ex.getMessage())
  //   // }
  // }
}
