scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" % "scalatest_native0.4_2.13" % "3.2.13"

enablePlugins(ScalaNativePlugin)

nativeLinkStubs := true

scalacOptions := Seq("-unchecked", "-deprecation")
