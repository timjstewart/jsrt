import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonToJavascriptExtractorSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  "An extractor" should "extract an empty array correctly" in {
    JsonToJavascriptExtractor.extract("[]").value shouldBe ("")
  }

  it should "extract two blocks of code" in {
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
/** path([0].top[0].jsCode) */
function func() {
    return true;
}

/** path([0].top[1].jsCode) */
function func() {
    return false;
}
""".trim)
  }

  it should "extract code with indentation" in {
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
/** path([0].jsCode) */
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
