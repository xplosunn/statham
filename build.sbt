val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "statham",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq(
        "-experimental",
        "-old-syntax",
    ),

    libraryDependencies ++= Seq(
        "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.7"
    ) ++ Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
    ).map(_ % "0.14.1"),

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
