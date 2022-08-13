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

  it should "return a two element array" in {
    Parser.parse("[1, 0]").value shouldBe (JArray(List(JNumber(1), JNumber(0))))
  }

  it should "return a two element number array when the comma is missing" in {
    Parser.parse("[1 0]").value shouldBe (JArray(List(JNumber(1), JNumber(0))))
  }

  it should "return a two element string array" in {
    Parser.parse("""["Tim","Stewart"]""").value shouldBe (JArray(
      List(JString("Tim"), JString("Stewart"))
    ))
  }

  it should "return a two element string array when the comma is missing" in {
    Parser.parse("""["Tim" "Stewart"]""").value shouldBe (JArray(
      List(JString("Tim"), JString("Stewart"))
    ))
  }

  it should "return an empty object" in {
    Parser.parse("{}").value shouldBe (JObject(Map()))
  }

  it should "return nested array" in {
    Parser.parse("[[]]").value shouldBe (JArray(List(JArray(List()))))
  }

  it should "return object nested in an array" in {
    Parser.parse("[{}]").value shouldBe (JArray(List(JObject(Map()))))
  }

  it should "return an object with a property" in {
    Parser.parse("""{"name": "Tim"}""").value shouldBe (JObject(
      Map("name" -> JString("Tim"))
    ))
  }

  it should "return an object with two properties" in {
    Parser.parse("""{"name": "Tim", "age": 50}""").value shouldBe (JObject(
      Map("name" -> JString("Tim"), "age" -> JNumber(50))
    ))
  }
}
