import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ch.ethz.math.ifor"
ThisBuild / organizationName := "ETH Zurich"

lazy val root = (project in file("."))
  .settings(
    name := "code",
    libraryDependencies += scalaTest % Test
  )

fork := true
run / javaOptions += f"-Djava.library.path=${sys.env("OR_TOOLS_HOME")}/lib"