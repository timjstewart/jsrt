import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JavascriptToJsonConverterSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  "A converter" should "merge a one line function" in {

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

  it should "merge a function with a nested block" in {

        JavaScriptToJsonConverter.merge("""
/** path(code) */
function foo() {
  if (true) {
    return 42;
  }
}
""".trim,
"""
{
    "code": "return 42;"
}
"""
        ).value should be(
"""
{
  "code": "if (true) {
  return 42;
}"
}
""".trim)
      }
}
