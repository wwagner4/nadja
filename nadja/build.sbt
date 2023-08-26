val scala3Version = "3.3.0"
val AkkaVersion = "2.8.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "nadja",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    resolvers += "Akka library repository".at("https://repo.akka.io/maven"),

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.9.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
    ),
    scalacOptions ++= Seq(
      // "-deprecation"
    ),
  )

