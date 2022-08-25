import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonPathSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  import json.path._


  "A single property path" should "be able to be parsed" in {
    JsonPath.parse("code").value.toString should be("code")
  }

  "A two property path" should "be able to be parsed" in {
    JsonPath.parse("code.text").value.toString should be("code.text")
  }

  "A two property path each with indexes" should "be able to be parsed" in {
    JsonPath.parse("code[0].text[1]").value.toString should be(
      "code[0].text[1]"
    )
  }

  "A two property path with indexes" should "be able to be parsed" in {
      JsonPath.parse("code.text[0][1]").value.toString should be(
        "code.text[0][1]"
      )
    }

}
