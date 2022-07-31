import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

import scala.io.Source

class LexerSpec extends AnyFlatSpec with should.Matchers {
 "A lexer" should "return EOF for an empty stream" in {
   Lexer.tokenize(Source.fromString("")).right.get shouldBe List(EOF(Position(1,1)))
 }

 it should "return COMMA, EOF" in {
    Lexer.tokenize(Source.fromString(",")).right.get shouldBe List(
      COMMA(Position(1,1)),
      EOF(Position(1,2))
    )
  }

 it should "return open and close square brackets, EOF" in {
     Lexer.tokenize(Source.fromString("[]")).right.get shouldBe List(
       OPEN_SQUARE_BRACKET(Position(1,1)),
       CLOSE_SQUARE_BRACKET(Position(1,2)),
       EOF(Position(1,3))
     )
   }
 it should "return open close square brackets on different lines, EOF" in {
      Lexer.tokenize(Source.fromString("[\n]")).right.get shouldBe List(
        OPEN_SQUARE_BRACKET(Position(1,1)),
        NEWLINE(Position(1,2)),
        CLOSE_SQUARE_BRACKET(Position(2,1)),
        EOF(Position(2,2))
      )
    }
}
