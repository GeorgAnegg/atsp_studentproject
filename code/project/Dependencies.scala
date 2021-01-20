import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"
  //lazy val sparkCore = "org.apache.spark" %% "spark-core" % "3.0.0" % "provided"
  //lazy val sparkPoi = "org.apache.poi" % "poi" % "3.17" % "provided"
  //lazy val sparkSql = "org.apache.spark" %% "spark-sql" % "3.0.0" % "provided"

  lazy val sparkPoi = "org.apache.poi" % "poi" % "3.15-beta2"
  lazy val sparkPoiO = "org.apache.poi" % "poi-ooxml" % "3.15-beta2"
  lazy val sparkPoiOS = "org.apache.poi" % "poi-ooxml-schemas" % "3.15-beta2"
  lazy val optimus = "com.github.vagmcs" %% "optimus" % "3.2.4"
  lazy val optimusLP = "com.github.vagmcs" %% "optimus-solver-lp" % "3.2.4"
}
