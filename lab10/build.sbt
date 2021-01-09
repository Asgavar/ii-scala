import scalariform.formatter.preferences._

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-Wunused",
)
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

ThisBuild / scalariformPreferences := scalariformPreferences.value
  .setPreference(IndentSpaces, 2)
  .setPreference(RewriteArrowSymbols, false)

ThisBuild / scalafixOnCompile := true
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision  // fall back to what scalafix specified as the default

ThisBuild / coverageEnabled := true
ThisBuild / coverageMinimum := 75
ThisBuild / coverageFailOnMinimum := true
ThisBuild / coverageHighlighting := true

lazy val core = project
  .settings(
    name := "Core"
  )

lazy val blackjack = project
  .dependsOn(core)
  .settings(
    name := "Blackjack"
  )
