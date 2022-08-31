import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

import json.path.pattern._

class JsonToJavascriptExtractorSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  def withPattern(patternString: String)(testFunc: List[Pattern] => Any): Unit = {
    Pattern.parse(patternString) match {
      case Right(pattern) => testFunc(List(pattern))
      case Left(error) => fail(error)
    }
  }

  "An extractor" should "extract an empty array correctly" in withPattern("**.jsCode") { patterns =>
    JsonToJavascriptExtractor.extract("[]", patterns).value shouldBe ("")
  }

  it should "extract two blocks of code" in withPattern("**.jsCode") { patterns =>
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
    JsonToJavascriptExtractor.extract(text, patterns).value shouldBe ("""
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

  it should "extract code with indentation" in withPattern("**.jsCode") { patterns =>
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
    JsonToJavascriptExtractor.extract(text, patterns).value shouldBe ("""
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

