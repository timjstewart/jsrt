package config

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class ConfigSpec extends AnyFlatSpec with should.Matchers with OptionValues {

  "An empty configuration" should "load" in {
    val Right(config) = Config.load("")
    config.getKeys() shouldBe empty
  }

  "An configuration with a comment" should "load" in {
    val Right(config) = Config.load("# this is a comment")
    config.getKeys() shouldBe empty
  }

  "An configuration with a value" should "load" in {
    val Right(config) = Config.load("name = Fred")
    config.getKeys() should be(Set("name"))
    config.getValue("name").value should be("Fred")
  }

  "An configuration with two values" should "load" in {
    val Right(config) = Config.load("""
# Fred's name
name = Fred
# Fred's age
age = 40
""")
    config.getKeys() should be(Set("name", "age"))
    config.getValue("name").value should be("Fred")
    config.getValue("age").value should be("40")
  }

  "An configuration" should "return None for a key that's not present" in {
    val Right(config) = Config.load("")
    config.getValue("name") should be(None)
  }
}
