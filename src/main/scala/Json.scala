package json

sealed abstract class JValue {
  def isObject: Boolean = false
  def isArray: Boolean = false
  def isNumber: Boolean = false
  def isBool: Boolean = false
  def isNull: Boolean = false
  def isString: Boolean = false

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
  override def isNull: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("null")
  }
}

case class JArray(elements: List[JValue]) extends JValue {
  override def isArray: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("[")
    if (elements.isEmpty) {
      sb.append("]")
    } else {
      // if we have any objects, print every element on its own line.
      val anyObjects = elements.exists(x => x.isObject)
      if (anyObjects) {
        sb.append("\n")
      }
      elements.zipWithIndex.foreach { x =>
        // between elements
        if (x._2 > 0) {
          if (anyObjects) {
            sb.append(",\n")
          } else {
            sb.append(", ")
          }
        }
        // right before the element
        if (anyObjects) {
          indent(sb, level + 1)
        }
        x._1.prettyPrintInternal(sb, level + 1)
      }
      // after all the elements
      if (anyObjects) {
        sb.append("\n")
        indent(sb, level)
      }
      sb.append("]")
    }
  }
}

case class JObject(properties: List[Tuple2[String, JValue]]) extends JValue {
  override def isObject: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("{")
    if (properties.isEmpty) {
      sb.append("}")
    } else {
      sb.append("\n")

      properties.zipWithIndex.foreach { x =>
        if (x._2 > 0) {
          sb.append(",\n")
        }
        indent(sb, level + 1)
        sb.append(""""%s": """.format(x._1._1))
        x._1._2.prettyPrintInternal(sb, level + 1)
      }
      sb.append("\n")
      indent(sb, level)
      sb.append("}")
    }
  }
}

case class JString(value: String) extends JValue {
  override def isString: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append(""""%s"""".format(value))
  }
}

case class JNumber(value: Double) extends JValue {
  override def isNumber: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("%s".format(value.toInt))
  }
}

case class JBool(value: Boolean) extends JValue {
  override def isBool: Boolean = true

  override def prettyPrintInternal(sb: StringBuffer, level: Int): Unit = {
    sb.append("%s".format(if (value) "true" else "false"))
  }
}

case class JProperty(obj: JObject, name: String) extends JValue {
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
