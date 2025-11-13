name := "shapes-scala-algebraic"

version := "0.2.0"

scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,
  "ch.qos.logback" % "logback-classic" % "1.4.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  
  // Droste recursion schemes library (Scala 3 compatible version)
  "io.higherkindness" %% "droste-core" % "0.9.0",
  
  // Cats functional programming library (required by Droste)
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.typelevel" %% "cats-free" % "2.10.0",
  "org.typelevel" %% "cats-laws" % "2.10.0" % Test,
  
  // ScalaCheck for property-based testing
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
  
  // Discipline for law testing
  "org.typelevel" %% "discipline-scalatest" % "2.2.0" % Test
)

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)