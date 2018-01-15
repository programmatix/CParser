enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"

lazy val CParser = (project in file("."))
  .settings(
    name := "CParser",
    scalaVersion := "2.12.4"
  )
