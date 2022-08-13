import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonToJavascriptConverterSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  "A converter" should "convert an empty array correctly" in {
    JsonToJavascriptConverter.convert("[]").value shouldBe ("")
  }

  "A converter" should "extract two blocks of code" in {
    val text = """
[
  {
    "top" : [
      {
          "jsCode": "return true;"
      },
      {
          "jsCode": "return false;"
      }
    ]
  }
]
"""
    print(JsonToJavascriptConverter.convert(text).value)
    JsonToJavascriptConverter.convert(text).value shouldBe ("""
/** ref([0].top[0].jsCode) */
function func() {
    return true;
}

/** ref([0].top[1].jsCode) */
function func() {
    return false;
}
""".trim)
  }
}
