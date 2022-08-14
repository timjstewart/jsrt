import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonToJavascriptExtractorSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  "A converter" should "extract an empty array correctly" in {
    JsonToJavascriptExtractor.extract("[]").value shouldBe ("")
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
    JsonToJavascriptExtractor.extract(text).value shouldBe ("""
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

  "A converter" should "extract code with indentation" in {
    val text = """
[
  {
    "jsCode": "
_.forEach(names, function(name) {
    if (name === null) {
        console.log('null name');
    }
});"
  ]
}
    """
    JsonToJavascriptExtractor.extract(text).value shouldBe ("""
/** ref([0].jsCode) */
function func() {
    _.forEach(names, function(name) {
        if (name === null) {
            console.log('null name');
        }
    });
}
    """.trim)
  }
}
