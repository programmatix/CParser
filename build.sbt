enablePlugins(ScalaJSPlugin)

// This is the only non-test dependency for the library
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.5.3" % "test"

lazy val CParser = (project in file("."))
  .settings(
    name := "CParser",
    scalaVersion := "2.12.4"
  )
