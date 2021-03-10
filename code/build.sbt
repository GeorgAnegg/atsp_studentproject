import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ch.ethz.math.ifor"
ThisBuild / organizationName := "ETH Zurich"

lazy val root = (project in file("."))
  .settings(
    name := "code",
    libraryDependencies += scalaTest % Test,
    //libraryDependencies += sparkCore,
    libraryDependencies += sparkPoi,
    libraryDependencies += sparkPoiO,
    libraryDependencies += sparkPoiOS,
    libraryDependencies ++= Seq(
          "com.github.vagmcs" %% "optimus" % "3.2.4",
          "com.github.vagmcs" %% "optimus-solver-oj" % "3.2.4",
          "com.github.vagmcs" %% "optimus-solver-lp" % "3.2.4"
    )
  )

fork := true
run / javaOptions += f"-Djava.library.path=${sys.env("OR_TOOLS_HOME")}/lib"
/*
scalacOptions ++= Seq(
  "-deprecation",
  //other options
)
 */
