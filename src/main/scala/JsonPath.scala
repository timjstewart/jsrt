
sealed abstract class Step(val text: String)
case class ArrayIndex(index: Int) extends Step("[%s]".format(index))
case class PropertyName(name: String) extends Step(".%s".format(name))

sealed case class JsonPath(steps: List[Step]) {
  override def toString = steps match {
    case Nil   => "null"
    case steps => steps.reverse.map(_.text).mkString("")
  }

  def ::(step: Step): JsonPath = this.copy(steps = step :: this.steps)
}

object JsonPath {
  def apply(steps: Step*): JsonPath = JsonPath(List(steps : _*))
}
