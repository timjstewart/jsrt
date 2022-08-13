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
    Parser.parse("{}").value shouldBe (JObject(Nil))
  }

  it should "return nested array" in {
    Parser.parse("[[]]").value shouldBe (JArray(List(JArray(List()))))
  }

  it should "return object nested in an array" in {
    Parser.parse("[{}]").value shouldBe (JArray(List(JObject(Nil))))
  }

  it should "return an object with a property" in {
    Parser.parse("""{"name": "Tim"}""").value shouldBe (JObject(
      List("name" -> JString("Tim"))
    ))
  }

  it should "return an object with a property with a missing colon" in {
    Parser.parse("""{"name" "Tim"}""").value shouldBe (JObject(
      List("name" -> JString("Tim"))
    ))
  }

  it should "return an object with two properties" in {
    Parser.parse("""{"name": "Tim", "age": 50}""").value shouldBe (JObject(
      List("name" -> JString("Tim"), "age" -> JNumber(50))
    ))
  }

  it should "return an object with two properties with missing colons and commas" in {
    Parser.parse("""{"name" "Tim" "age" 50}""").value shouldBe (JObject(
      List("name" -> JString("Tim"), "age" -> JNumber(50))
    ))
  }

  it should "return an error for an object with missing property value" in {
    Parser.parse("""{"status": """).left.value should include(
      "never given a value"
    )
  }

  it should "return an object for an object with missing closing curly brace" in {
    Parser.parse("""{"status": true""").value shouldBe (JObject(
      List("status" -> JBool(true))
    ))
  }

  it should "return a list of objects when the comma is missing" in {
    Parser.parse("""[{}{}]""").value shouldBe (JArray(
      List(JObject(Nil), JObject(Nil))
    ))
  }

  it should "return a list of arrays when the comma is missing" in {
    Parser.parse("""[[][]]""").value shouldBe (JArray(
      List(JArray(Nil), JArray(Nil))
    ))
  }

  it should "return a list of nulls when the comma is missing" in {
    Parser.parse("""[null null]""").value shouldBe (
      JArray(List(JNull, JNull))
    )
  }

  it should "parse a complex object" in {
    val text = """
[
  {
    "name": {
        "first": "Fred",
        "last": "Flintstone"
    },
    "hobbies": [],
    "age": 40,
    "id": null
  },
  {
      "name": {
          "first": "Wilma",
          "last": "Flintstone"
      },
      "hobbies": ["reading"],
      "age": 35,
      "id": "123456"
  }
]
"""
    Parser.parse(text).value shouldBe (
      JArray(
        List(
          JObject(
            List(
              "name" -> JObject(
                List(
                  "first" -> JString("Fred"),
                  "last" -> JString("Flintstone")
                )
              ),
              "hobbies" -> JArray(Nil),
              "age" -> JNumber(40),
              "id" -> JNull
            )
          ),
          JObject(
            List(
              "name" -> JObject(
                List(
                  "first" -> JString("Wilma"),
                  "last" -> JString("Flintstone")
                )
              ),
              "hobbies" -> JArray(List(JString("reading"))),
              "age" -> JNumber(35),
              "id" -> JString("123456")
            )
          )
        )
      )
    )
  }
}
