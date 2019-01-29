lazy val Benchmark = config("bench") extend Test

val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.8.2" % "bench"
val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

lazy val benchmarkSettings = Seq(
  libraryDependencies += scalaMeter,
  testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
  parallelExecution in Benchmark := false,
  logBuffered := false
)

lazy val `radix-tree` = project.in(file("."))
  .settings(
    scalaVersion := "2.12.8",
    libraryDependencies ++= Seq(
      scalaTest,
      scalaCheck
    ),
    benchmarkSettings
  )
  .configs(
    Benchmark
  )
  .settings(
    inConfig(Benchmark)(Defaults.testSettings): _*,
  )