package text

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._


class TextSpec extends AnyFlatSpec with should.Matchers with OptionValues {

  import text.Text._

  "A left-aligned string" should "not be indented" in {
    val text = "foo"
    text.unindent() should be(text)
  }

  "An indented string" should "be indented" in {
      " foo".unindent() should be("foo")
    }

  "An indented multi-line string" should "be indented" in {
    """    one
      two
        three""".unindent() should be(
      """one
  two
    three""".trim)
      }
}
