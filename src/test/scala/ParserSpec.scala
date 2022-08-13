import org.scalatest._
import flatspec._
import matchers._
import org.scalatest.matchers.should._

class ParserSpec extends AnyFlatSpec with should.Matchers with EitherValues {

  import Parser._

  "A Parser" should "return an empty array" in {
    Parser.parse("[]").value shouldBe(JArray(List())) 
  }

  it should "return an empty object" in {
      Parser.parse("{}").value shouldBe(JObject(Map())) 
    }
}
