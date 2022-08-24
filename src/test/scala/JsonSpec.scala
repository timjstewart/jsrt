package json

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonSpec extends AnyFlatSpec with should.Matchers {

  "An empty Array" should "be pretty printed correctly" in {
    JArray().prettyPrint should be("[]")
  }

  "An empty Object" should "be pretty printed correctly" in {
    JObject().prettyPrint should be("{}")
  }

  "A true boolean" should "be pretty printed correctly" in {
    JBool(true).prettyPrint should be("true")
  }

  "A false boolean" should "be pretty printed correctly" in {
    JBool(false).prettyPrint should be("false")
  }

  "A whole number" should "be pretty printed correctly" in {
    JNumber(123).prettyPrint should be("123")
  }

  "An single element Array" should "be pretty printed correctly" in {
    JArray(JBool(true)).prettyPrint should be("[true]")
  }

  "An two element Array" should "be pretty printed correctly" in {
    JArray(JBool(true), JBool(false)).prettyPrint should be("[true, false]")
  }

  "An three element Array" should "be pretty printed correctly" in {
    JArray(JBool(true), JBool(false), JNumber(1)).prettyPrint should be(
      "[true, false, 1]"
    )
  }

  "An Object with a property" should "be pretty printed correctly" in {
    JObject(List("name" -> JString("Tim"))).prettyPrint should be("""{
  "name": "Tim"
}""")
  }

  "An Object with two properties" should "be pretty printed correctly" in {
    JObject(
      List("name" -> JString("Tim"), "age" -> JNumber(49))
    ).prettyPrint should be("""
{
  "name": "Tim",
  "age": 49
}
""".trim)
  }

}
