import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

import scala.io.Source

class LexerSpec extends AnyFlatSpec with should.Matchers {

  "A lexer" should "return EOF for an empty stream" in {
    Lexer.tokenize(Source.fromString("")).right.get shouldBe List(
      EOF(Position(1, 1))
    )
  }

  it should "return COMMA, EOF" in {
    Lexer.tokenize(Source.fromString(",")).right.get shouldBe List(
      COMMA(Position(1, 1)),
      EOF(Position(1, 2))
    )
  }

  it should "return open and close square brackets, EOF" in {
    Lexer.tokenize(Source.fromString("[]")).right.get shouldBe List(
      OPEN_SQUARE_BRACKET(Position(1, 1)),
      CLOSE_SQUARE_BRACKET(Position(1, 2)),
      EOF(Position(1, 3))
    )
  }

  it should "return open close square brackets on different lines, EOF" in {
    Lexer.tokenize(Source.fromString("[\n]")).right.get shouldBe List(
      OPEN_SQUARE_BRACKET(Position(1, 1)),
      NEWLINE(Position(1, 2)),
      CLOSE_SQUARE_BRACKET(Position(2, 1)),
      EOF(Position(2, 2))
    )
  }

  it should "return an empty string" in {
    Lexer.tokenize(Source.fromString("\"\"")).right.get shouldBe List(
      STRING("", Position(1, 1)),
      EOF(Position(1, 3))
    )
  }

  it should "return a non-empty string" in {
    Lexer.tokenize(Source.fromString("\"test\"")).right.get shouldBe List(
      STRING("test", Position(1, 1)),
      EOF(Position(1, 7))
    )
  }

  it should "return a non-empty string containing a space" in {
    Lexer.tokenize(Source.fromString("\"test one\"")).right.get shouldBe List(
      STRING("test one", Position(1, 1)),
      EOF(Position(1, 11))
    )
  }

  it should "return a string with a escaped double quote" in {
    Lexer.tokenize(Source.fromString(""""foo\"bar"""")).right.get shouldBe List(
      STRING("""foo\"bar""", Position(1, 1)),
      EOF(Position(1, 11))
    )
  }

  it should "return a string containing a quoted string" in {
    Lexer
      .tokenize(Source.fromString(""""foo\"bar\"baz""""))
      .right
      .get shouldBe List(
      STRING("""foo\"bar\"baz""", Position(1, 1)),
      EOF(Position(1, 16))
    )
  }

  it should "return a string even if the quote is never closed" in {
    Lexer.tokenize(Source.fromString(""""foo""")).right.get shouldBe List(
      STRING("foo", Position(1, 1)),
      EOF(Position(1, 5))
    )
  }

  it should "return a true value" in {
    Lexer.tokenize(Source.fromString("true")).right.get shouldBe List(
      BOOL_TRUE(Position(1, 1)),
      EOF(Position(1, 5))
    )
  }

  it should "return a false value" in {
    Lexer.tokenize(Source.fromString("false")).right.get shouldBe List(
      BOOL_FALSE(Position(1, 1)),
      EOF(Position(1, 6))
    )
  }

  // it should "return a whole number" in {
  //         Lexer.tokenize(Source.fromString("123")).right.get shouldBe List(
  //           NUMBER(123.0, Position(1, 1)),
  //           EOF(Position(1, 4))
  //         )
  //       }
}
