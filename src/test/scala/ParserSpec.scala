import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class ParserSpec extends AnyFlatSpec with should.Matchers with EitherValues {

  import Parser._

  "A Parser" should "return an empty array" in {
    Parser.parse("[]").value shouldBe (JArray())
  }

  it should "return a single element array" in {
    println("TEST: %s".format(Parser.parse("[1]")))
    Parser.parse("[1]").value shouldBe (JArray(JNumber(1)))
  }

  it should "return a two element array" in {
    Parser.parse("[1, 0]").value shouldBe (JArray(JNumber(1), JNumber(0)))
  }

  it should "return a two element number array when the comma is missing" in {
    Parser.parse("[1 0]").value shouldBe (JArray(JNumber(1), JNumber(0)))
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
    Parser.parse("{}").value shouldBe (JObject())
  }

  it should "return nested array" in {
    Parser.parse("[[]]").value shouldBe (JArray(JArray()))
  }

  it should "return object nested in an array" in {
    Parser.parse("[{}]").value shouldBe (JArray(JObject()))
  }

  it should "return an object with a property" in {
    Parser.parse("""{"name": "Tim"}""").value shouldBe (JObject(
      "name" -> JString("Tim")
    ))
  }

  it should "return an object with a property with a missing colon" in {
    Parser.parse("""{"name" "Tim"}""").value shouldBe (JObject(
      "name" -> JString("Tim")
    ))
  }

  it should "return an object with two properties" in {
    Parser.parse("""{"name": "Tim", "age": 50}""").value shouldBe (JObject(
      "name" -> JString("Tim"),
      "age" -> JNumber(50)
    ))
  }

  it should "return an object with two properties with missing colons and commas" in {
    Parser.parse("""{"name" "Tim" "age" 50}""").value shouldBe (JObject(
      "name" -> JString("Tim"),
      "age" -> JNumber(50)
    ))
  }

  it should "return an error for an object with missing property value" in {
    Parser.parse("""{"status": """).left.value should include(
      "never given a value"
    )
  }

  it should "return an object for an object with missing closing curly brace" in {
    Parser.parse("""{"status": true""").value shouldBe (JObject(
      "status" -> JBool(true)
    ))
  }

  it should "return a list of objects when the comma is missing" in {
    Parser.parse("""[{}{}]""").value shouldBe (JArray(
      List(JObject(), JObject())
    ))
  }

  it should "return a list of arrays when the comma is missing" in {
    Parser.parse("""[[][]]""").value shouldBe (JArray(
      List(JArray(Nil), JArray(Nil))
    ))
  }

  it should "return a list of nulls when the comma is missing" in {
    Parser.parse("""[null null]""").value shouldBe (
      JArray(JNull, JNull)
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
            "name" -> JObject(
              "first" -> JString("Fred"),
              "last" -> JString("Flintstone")
            ),
            "hobbies" -> JArray(Nil),
            "age" -> JNumber(40),
            "id" -> JNull
          ),
          JObject(
            "name" -> JObject(
              "first" -> JString("Wilma"),
              "last" -> JString("Flintstone")
            ),
            "hobbies" -> JArray(JString("reading")),
            "age" -> JNumber(35),
            "id" -> JString("123456")
          )
        )
      )
    )
  }

  it should "parse two equivalent objects the same" in {
    val clean = """
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
    val messy = """
[
  {
    "name": {
        "first" "Fred"
        "last" "Flintstone"
    },
    "hobbies": [],
    "age": 40
    "id": null
  },
  { "name":{"first":"Wilma","last":"Flintstone"},
"hobbies":["reading"],
"age": 35,
"id":"123456" }]
"""
    Parser.parse(clean) should be(Parser.parse(messy))
  }

  it should "return a list of nulls when there is a trailing comma" in {
    Parser.parse("""[null null,]""").value shouldBe (
      JArray(JNull, JNull)
    )
  }

  it should "return an error when parsing an extra colon" in {
    Parser.parse(""""{ "prop1" : "value" : "prop2" }""").left.value should include("fail")
  }
}
