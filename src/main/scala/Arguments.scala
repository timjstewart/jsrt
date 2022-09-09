package arguments

sealed case class Arguments(files: List[String], configFile: Option[String])

object Arguments {
  def parse(args: Array[String]): Either[String, Arguments] = {
    val configFileFlag = "--config="
    Right(args.foldLeft(Arguments(Nil, None)) { (args, arg) =>
      if (arg.startsWith(configFileFlag)) {
        val fileName = arg.substring(configFileFlag.length())
        args.copy(configFile = Some(fileName))
      } else {
        args.copy(files = args.files :+ arg)
      }
    })
  }
}
