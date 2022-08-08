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
      if (!cond(this)) {
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
            x
          case c => Left("lexer fail: %s".format(c))
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        Tuple2(tokens :+ EOF(remaining), remaining)
      }
  }

  private def tokenizeString(startPos: Buffer): Result = {
    val str = new StringBuffer("\"")
    var escaped = false
    var done = false
    startPos
      .advance() // pass the first double quote
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
            done = true
            str.append('"')
            success(buffer.advance())
          case Some(c) =>
            str.append(c)
            success(buffer.advance())
          case None =>
            str.append('"')
            done = true
            success(buffer.advance())
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        Tuple2(
          tokens :+ StringToken(str.toString, startPos),
          remaining.advance()
        )
      }
  }
}
