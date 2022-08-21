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

  "A single property path" should "be able to be parsed" in {
    JsonPath.parse("code").value.toString should be("code")
  }

  "A two property path" should "be able to be parsed" in {
    JsonPath.parse("code.text").value.toString should be("code.text")
  }

  "A two property path each with indexes" should "be able to be parsed" in {
    JsonPath.parse("code[0].text[1]").value.toString should be(
      "code[0].text[1]"
    )
  }

  "A two property path with indexes" should "be able to be parsed" in {
      JsonPath.parse("code.text[0][1]").value.toString should be(
        "code.text[0][1]"
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

  "A pattern with an index wild card then some properties" should "should match" in {
    val path = JsonPath(
      PropertyName("name"),
      PropertyName("blocks"),
      PropertyName("top"),
      ArrayIndex(0)
    )
    Pattern.parse("[].top.blocks.name").value.matches(path) should be(true)
  }

  "A pattern with an index then some properties" should "should match" in {
    val path = JsonPath(
      PropertyName("name"),
      PropertyName("blocks"),
      PropertyName("top"),
      ArrayIndex(0)
    )
    Pattern.parse("[0].top.blocks.name").value.matches(path) should be(true)
  }

  "A pattern with an index then some properties" should "parses correctly" in {
    Pattern.parse("[0].top.blocks.name").value should be(
      Pattern(Index(0), Property("top"), Property("blocks"), Property("name"))
    )
  }

  "A pattern with two indexes then some properties" should "parses correctly" in {
    Pattern.parse("[0][1].top.blocks.name").value should be(
      Pattern(
        Index(0),
        Index(1),
        Property("top"),
        Property("blocks"),
        Property("name")
      )
    )
  }

  "A pattern with an index wildcard then an index then some properties" should "parses correctly" in {
    Pattern.parse("[][1].top.blocks.name").value should be(
      Pattern(
        IndexWildCard,
        Index(1),
        Property("top"),
        Property("blocks"),
        Property("name")
      )
    )
  }

  "A pattern with an index then an index wildcard then some properties" should "parses correctly" in {
    Pattern.parse("[1][].top.blocks.name").value should be(
      Pattern(
        Index(1),
        IndexWildCard,
        Property("top"),
        Property("blocks"),
        Property("name")
      )
    )
  }

  "A pattern with an index wild card then some properties" should "parses correctly" in {
    Pattern.parse("[].top.blocks.name").value should be(
      Pattern(
        IndexWildCard,
        Property("top"),
        Property("blocks"),
        Property("name")
      )
    )
  }
}
