import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JavascriptToJsonConverterSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  "A converter" should "merge code" in {

    JavaScriptToJsonConverter.merge(
      """
/** path(code) */
function foo() {
  return 42;
}
""",
      """
{
   "code": ""
}
"""
    ).value should be(
"""
{
  "code": "return 42;"
}
""".trim)
  }
}
