import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

import scala.io.Source

class LexerSpec extends AnyFlatSpec with should.Matchers {

  import Lexer._

  "A lexer" should "return EOF for an empty stream" in {
    Lexer.tokenize("").right.get._1 shouldBe List(
      EOF(Buffer("", 1, 1))
    )
  }

  it should "return COMMA, EOF" in {
    val text = ","
    Lexer.tokenize(text).right.get._1 shouldBe List(
      CommaToken(Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 2, 1))
    )
  }

  it should "return open and close square brackets, EOF" in {
    val text = "[]"
    Lexer.tokenize(text).right.get._1 shouldBe List(
      OpenSquareBracketToken(Buffer(text, 1, 1, 0)),
      CloseSquareBracketToken(Buffer(text, 1, 2 ,1)),
      EOF(Buffer(text, 1, 3, 2))
    )
  }

  it should "return open close square brackets on different lines, EOF" in {
    val text = "[\n]"
    Lexer.tokenize(text).right.get._1 shouldBe List(
      OpenSquareBracketToken(Buffer(text, 1, 1, 0)),
      NewLineToken(Buffer(text, 1, 2, 1)),
      CloseSquareBracketToken(Buffer(text, 2, 1, 2)),
      EOF(Buffer(text, 2, 2, 3))
    )
  }

  it should "return an empty string" in {
    val text = "\"\""
    Lexer.tokenize(text).right.get._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 3, 2))
    )
  }

  it should "return a non-empty string" in {
    val text = "\"test\""
    Lexer.tokenize(text).right.get._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 7, 6))
    )
  }

  it should "return a non-empty string containing a space" in {
    val text = "\"test one\""
    Lexer.tokenize(text).right.get._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 11, 10))
    )
  }

  it should "return a string with a escaped double quote" in {
    val text = """"foo\"bar""""
    Lexer.tokenize(text).right.get._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 11, 10))
    )
  }

  // it should "return a string containing a quoted string" in {
  //   Lexer
  //     .tokenize(Source.fromString(""""foo\"bar\"baz""""))
  //     .right
  //     .get shouldBe List(
  //     STRING("""foo\"bar\"baz""", Buffer(1, 1)),
  //     EOF(Buffer(1, 16))
  //   )
  // }

  // it should "return a string even if the quote is never closed" in {
  //   Lexer.tokenize(Source.fromString(""""foo""")).right.get shouldBe List(
  //     STRING("foo", Buffer(1, 1)),
  //     EOF(Position(1, 5))
  //   )
  // }

  // it should "return a true value" in {
  //   Lexer.tokenize(Source.fromString("true")).right.get shouldBe List(
  //     BOOL_TRUE(Position(1, 1)),
  //     EOF(Position(1, 5))
  //   )
  // }

  // it should "return a false value" in {
  //   Lexer.tokenize(Source.fromString("false")).right.get shouldBe List(
  //     BOOL_FALSE(Position(1, 1)),
  //     EOF(Position(1, 6))
  //   )
  // }

  // // it should "return a whole number" in {
  // //         Lexer.tokenize(Source.fromString("123")).right.get shouldBe List(
  // //           NUMBER(123.0, Position(1, 1)),
  // //           EOF(Position(1, 4))
  // //         )
  // //       }
}
