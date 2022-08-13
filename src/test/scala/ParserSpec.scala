import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class ParserSpec extends AnyFlatSpec with should.Matchers with EitherValues {

  import Parser._

  "A Parser" should "return an empty array" in {
    Parser.parse("[]").value shouldBe (JArray(List()))
  }

  it should "return a single element array" in {
    println("TEST: %s".format(Parser.parse("[1]")))
    Parser.parse("[1]").value shouldBe (JArray(List(JNumber(1))))
  }

  it should "return an empty object" in {
    Parser.parse("{}").value shouldBe (JObject(Map()))
  }
}
