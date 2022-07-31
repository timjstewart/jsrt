// scalaVersion := "2.11.12"

// // libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
// // libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"

// libraryDependencies += "org.scalatest" % "scalatest_native0.3_2.11" % "3.2.0-SNAP10"

// enablePlugins(ScalaNativePlugin)
// nativeLinkStubs := true

scalaVersion := "2.13.8"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

libraryDependencies += "org.scalatest" % "scalatest_native0.4_2.13" % "3.2.13"
