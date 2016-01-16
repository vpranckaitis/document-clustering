import sbt._

lazy val commonSettings = Seq(
  organization := "lt.vpranckaitis",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

val slick = Seq(
  "com.typesafe.slick" %% "slick" % "3.1.1",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.1.1"
)
val scalaScraper = "net.ruippeixotog" %% "scala-scraper" % "0.1.2"
val postgreSqlDriver = "org.postgresql" % "postgresql" % "9.4.1207.jre7"
val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
val logback = "ch.qos.logback" % "logback-classic" % "1.1.3"


val storage = (project in file("storage"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= slick ++ Seq(postgreSqlDriver, scalaLogging, logback))

val scraper = (project in file("scraper"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(scalaScraper, scalaLogging, logback))
  .dependsOn(storage)

val `document-clustering` = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(storage, scraper)