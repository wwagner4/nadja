val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "nadja",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    ),
    scalacOptions ++= Seq(
      // "-deprecation"
    ),
  )

