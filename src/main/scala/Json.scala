package json

sealed abstract class JValue {

  def prettyPrint: String = {
    val sb = new StringBuffer()
    val level = 0
    prettyPrintInternal(sb, level)
    sb.toString
  }

  protected[json] def prettyPrintInternal(sb: StringBuffer, level: Int): Unit

  protected def indent(sb: StringBuffer, level: Int): Unit = {
    for (_ <- 0 until level) {
      sb.append("  ")
    }
  }
}

case object JNull extends JValue {
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("null")
  }
}
case class JArray(elements: List[JValue]) extends JValue {
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("[")
    if (elements.isEmpty) {
      sb.append("]")
    } else {
      sb.append("\n")
      elements.foreach(x => x.prettyPrintInternal(sb, level + 1))
      indent(sb, level)
      sb.append("]")
    }
  }
}

case class JObject(properties: List[Tuple2[String, JValue]]) extends JValue{
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("{")
        if (properties.isEmpty) {
          sb.append("}")
        } else {
          sb.append("\n")
          properties.foreach{prop =>
            indent(sb, level + 1)
            sb.append(""""%s":""".format(prop._1))
            prop._2.prettyPrintInternal(sb, level + 1)
          }
          sb.append("}")
        }
  }
}

case class JString(value: String) extends JValue{
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append(""""%s"""".format(value))
  }
}

case class JNumber(value: Double) extends JValue{
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("%s".format(value))
  }
}

case class JBool(value: Boolean) extends JValue{
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("%s".format(if (value) "true" else "false"))
  }
}

case class JProperty(obj: JObject, name: String) extends JValue{
  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = ???
}

object JArray {
  def apply(jValues: JValue*): JArray = JArray(List(jValues: _*))
}

object JObject {
  def apply(properties: Tuple2[String, JValue]*): JObject = JObject(
    List(properties: _*)
  )
}
