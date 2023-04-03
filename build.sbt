//ThisBuild / scalaVersion := "2.13.3"
//ThisBuild / organization := "es.david"
//ThisBuild / version := "0.0.1-SNAPSHOT"
//ThisBuild / fork := true

val Http4sVersion = "0.23.18"
val CirceVersion = "0.14.3"
val MunitVersion = "0.7.29"
val LogbackVersion = "1.2.11"
val MunitCatsEffectVersion = "1.0.7"

val CatsVersion = "2.2.0"
val CatsEffectVersion = "2.2.0"
val CatsTaglessVersion = "0.11"

val commonSettings =
  Seq(
    organization := "es.david",
    addCompilerPlugin(
      "org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % MunitVersion % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val scalaHttp4s000 = (project in file("000-curso-udemy"))
  .settings(commonSettings)
  .settings(
    name := "000-curso-udemy",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.10",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-laws" % "2.1.1",
      "org.typelevel" %% "discipline-core" % "1.0.0",
      "org.typelevel" %% "discipline-scalatest" % "2.1.1",
      "org.scalatest" %% "scalatest" % "3.2.2"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("munit.Framework"),
    scalacOptions --= Seq(
      "-Xfatal-warnings"
    )
  )
//enablePlugins(JavaAppPackaging)

