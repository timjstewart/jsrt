package arguments

import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class ArgumentsSpec extends AnyFlatSpec with should.Matchers with EitherValues {

  "Zero arguments" should "result in empty object" in {
    Arguments.parse(Array.empty[String]).value should be(
      Arguments(Nil, None)
    )
  }

  "A config file" should "result in a config file" in {
    Arguments.parse(Array("--config=foo.conf")).value should be(
      Arguments(Nil, Some("foo.conf"))
    )
  }

  "A list of files" should "result in an List of files" in {
    Arguments.parse(Array("one.json", "two.json")).value should be(
      Arguments(List("one.json", "two.json"), None)
    )
  }

  "A config file and list of files" should "result in an List of files and a config file" in {
      Arguments.parse(Array("one.json", "two.json", "--config=foo.conf")).value should be(
        Arguments(List("one.json", "two.json"), Some("foo.conf"))
      )
    }
}
