object Lexer {

  import scala.io.Source

  val True = "true"
  val False = "false"

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
        // returning this means that advance had no effect.
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

    def advance(by: String): Result = {
      if (text.substring(index).startsWith(by)) {
        var res = this
        for (c <- by) {
          res = res.advance()
        }
        Right(Tuple2(List(GenericToken(by, this)), res))
      } else {
        Left("expected %s in %s".format(by, this))
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
  case class BoolTrueToken(pos: Buffer) extends Token(True, pos)
  case class BoolFalseToken(pos: Buffer) extends Token(False, pos)
  case class NumberToken(value: Double, pos: Buffer)
      extends Token(value.toString, pos)
  case class GenericToken(value: String, pos: Buffer) extends Token(value, pos)

  case class EOF(pos: Buffer) extends Token("EOF", pos)

  class LexerError(message: String) extends Exception(message)

  def makeSpecific(result: Tuple2[List[Token], Buffer]): Result = result match {
    case Tuple2(GenericToken(`True`, pos) :: Nil, buf) =>
      Right(Tuple2(List(BoolTrueToken(pos)), buf))
    case Tuple2(GenericToken(`False`, pos) :: Nil, buf) =>
      Right(Tuple2(List(BoolFalseToken(pos)), buf))
    case _ => Left("Could not make more specific")
  }

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
          case Some('"')  => tokenizeString(b)
          case Some('t')  => tokenizeBool(b)
          case Some('f')  => tokenizeBool(b)
          case Some(c) if c.isDigit => tokenizeNumber(b)
          case c                    => Left("lexer fail: %s".format(c))
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        Tuple2(tokens :+ EOF(remaining), remaining)
      }
  }

  private def tokenizeNumber(startPos: Buffer): Result = {
    var accumulator = 0.0
    var done = false
    startPos.scanWhile(_ => !done) { buffer =>
      buffer.currentChar match {
        case Some(c) if (c.isDigit) =>
          accumulator *= 10
          accumulator += c.asDigit
          Right(Tuple2(List.empty[Token], buffer.advance()))
        case _ =>
          done = true
          Right(Tuple2(List(NumberToken(accumulator, startPos)), buffer.advance()))
      }
    }
  }

  private def tokenizeBool(startPos: Buffer): Result = {
    startPos.currentChar
      .map { c =>
        if (c == 't') {
          startPos.advance(True).flatMap(x => makeSpecific(x))
        } else if (c == 'f') {
          startPos.advance(False).flatMap(x => makeSpecific(x))
        } else {
          Left("Not a bool")
        }
      }
      .getOrElse(Left("Could not tokenize bool"))
  }

  private def tokenizeString(startPos: Buffer): Result = {
    val str = new StringBuffer("\"")
    var escaped = false
    var done = false
    var terminated = false
    startPos
      .advance() // pass the first double quote
      .scanWhile(_ => !done) { buffer =>
        buffer.currentChar match {
          // Escape character
          case Some('\\') =>
            escaped = true
            str.append('\\')
            success(buffer.advance())
          // Escaped double quote
          case Some('"') if escaped =>
            str.append('"')
            escaped = false
            success(buffer.advance())
          // Unescaped double quote
          case Some('"') =>
            str.append('"')
            done = true
            terminated = true
            success(buffer.advance())
          // All other characters
          case Some(c) =>
            str.append(c)
            success(buffer.advance())
          // End of string
          case None =>
            done = true
            terminated = true
            str.append('"')
            success(buffer.advance())
        }
      }
      .map { case Tuple2(tokens, remaining) =>
        if (!terminated) str.append('"')
        Tuple2(
          tokens :+ StringToken(str.toString, startPos),
          remaining.advance()
        )
      }
  }
}
