import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

import scala.io.Source

class LexerSpec extends AnyFlatSpec with should.Matchers with EitherValues {

  import Lexer._

  "A lexer" should "return EOF for an empty stream" in {
    Lexer.tokenize("").value._1 shouldBe List(
      EOF(Buffer("", 1, 1))
    )
  }

  it should "return COMMA, EOF" in {
    val text = ","
    Lexer.tokenize(text).value._1 shouldBe List(
      CommaToken(Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 2, 1))
    )
  }

  it should "return open and close square brackets, EOF" in {
    val text = "[]"
    Lexer.tokenize(text).value._1 shouldBe List(
      OpenSquareBracketToken(Buffer(text, 1, 1, 0)),
      CloseSquareBracketToken(Buffer(text, 1, 2, 1)),
      EOF(Buffer(text, 1, 3, 2))
    )
  }

  it should "return open close square brackets on different lines, EOF" in {
    val text = "[\n]"
    Lexer.tokenize(text).value._1 shouldBe List(
      OpenSquareBracketToken(Buffer(text, 1, 1, 0)),
      NewLineToken(Buffer(text, 1, 2, 1)),
      CloseSquareBracketToken(Buffer(text, 2, 1, 2)),
      EOF(Buffer(text, 2, 2, 3))
    )
  }

  it should "return an empty string" in {
    val text = "\"\""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 3, 2))
    )
  }

  it should "return a non-empty string" in {
    val text = "\"test\""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 7, 6))
    )
  }

  it should "return a non-empty string containing a space" in {
    val text = "\"test one\""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 11, 10))
    )
  }

  it should "return a string with a escaped double quote" in {
    val text = """"foo\"bar""""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 11, 10))
    )
  }

  it should "return a string containing a quoted string" in {
    val text = """"foo\"bar\"baz""""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(text, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 16, 15))
    )
  }

  it should "return a string even if the quote is never closed" in {
    val text = """"foo"""
    val result = """"foo""""
    Lexer.tokenize(text).value._1 shouldBe List(
      StringToken(result, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 5, 4))
    )
  }

  it should "return a true value" in {
    val text = "true"
    Lexer.tokenize(text).value._1 shouldBe List(
      BoolTrueToken(Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 5, 4))
    )
  }

  it should "return a false value" in {
    val text = "false"
    println(Lexer.tokenize(text))
    Lexer.tokenize(text).value._1 shouldBe List(
      BoolFalseToken(Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 6, 5))
    )
  }

  it should "return a single digit whole number" in {
    val text = "1"
    Lexer.tokenize(text).value._1 shouldBe List(
      NumberToken(1.0, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 2, 1))
    )
  }

  it should "return a 0" in {
    val text = "0"
    Lexer.tokenize(text).value._1 shouldBe List(
      NumberToken(0.0, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 2, 1))
    )
  }

  it should "return a whole number" in {
    val text = "123"
    Lexer.tokenize(text).value._1 shouldBe List(
      NumberToken(123.0, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 4, 3))
    )
  }

  it should "return a whole number followed by a space" in {
    val text = "123 "
    Lexer.tokenize(text).value._1 shouldBe List(
      NumberToken(123.0, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 5, 4))
    )
  }

  it should "fail if true is misspelled" in {
    val text = "tru"
    Lexer.tokenize(text).left.value should startWith("expected true")
  }

  it should "fail if false is misspelled" in {
    val text = "fasle"
    Lexer.tokenize(text).left.value should startWith("expected false")
  }

  it should "return a negative whole number" in {
    val text = "-123"
    Lexer.tokenize(text).value._1 shouldBe List(
      NumberToken(-123.0, Buffer(text, 1, 1, 0)),
      EOF(Buffer(text, 1, 5, 4))
    )
  }

  it should "return tokens for an array" in {
    val text = "[ 1, 2,3]"
    println(Lexer.tokenize(text))
  }
}
