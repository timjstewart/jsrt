import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class JsonPatternSpec
    extends AnyFlatSpec
    with should.Matchers
    with EitherValues {

  import json.path._
  import json.path.pattern._

  "A path string" should "parse into a pattern" in {
    Pattern.parse("top").value should be(Pattern(Property("top")))
  }

  "A path string with two steps" should "parse into a pattern with two parts" in {
    Pattern.parse("outer.inner").value should be(
      Pattern(Property("outer"), Property("inner"))
    )
  }

  "A path string with an index wildcard step" should "parse into a pattern with an index pattern" in {
    Pattern.parse("outer[]").value should be(
      Pattern(Property("outer"), IndexWildCard)
    )
  }

  "A path string with an index step" should "parse into a pattern with an index pattern" in {
    Pattern.parse("outer[1]").value should be(
      Pattern(Property("outer"), Index(1))
    )
  }

  "A path string with two index steps" should "parse into a pattern with two index patterns" in {
    Pattern.parse("outer[1][12]").value should be(
      Pattern(Property("outer"), Index(1), Index(12))
    )
  }

  "A path string with two index steps and an index wildcard" should "parse into a pattern with two index patterns and an index wildcard" in {
    Pattern.parse("outer[2][42][]").value should be(
      Pattern(Property("outer"), Index(2), Index(42), IndexWildCard)
    )
  }

  "A complex path string" should "parse correctly" in {
    Pattern.parse("outer[][0].middle[1].inner[32]").value should be(
      Pattern(
        Property("outer"),
        IndexWildCard,
        Index(0),
        Property("middle"),
        Index(1),
        Property("inner"),
        Index(32)
      )
    )
  }

  "A pattern" should "match a property path" in {
    val path = JsonPath(PropertyName("prop"))
    Pattern.parse("prop").value.matches(path) should be(true)
  }

  "A two property pattern" should "match a property path" in {
    val path = JsonPath(PropertyName("inner"), PropertyName("outer"))
    Pattern.parse("outer.inner").value.matches(path) should be(true)
  }

  "A pattern" should "should not match a property path with a different property name" in {
    val path = JsonPath(PropertyName("prop"))
    Pattern.parse("outer").value.matches(path) should be(false)
  }

  "A pattern with an index wild card" should "should match a property path" in {
    val path = JsonPath(ArrayIndex(3), PropertyName("prop"))
    Pattern.parse("prop[]").value.matches(path) should be(true)
  }

  "A pattern with a property wild card" should "should match a property path" in {
      val path = JsonPath(ArrayIndex(3), PropertyName("prop"))
      Pattern.parse("*[]").value.matches(path) should be(true)
    }
}
