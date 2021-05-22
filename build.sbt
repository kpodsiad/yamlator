val scala3Version = "3.0.0-RC3"
val projectName = "scala3-yaml"
lazy val root = project
  .in(file("."))
  .settings(
    name := projectName,
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
