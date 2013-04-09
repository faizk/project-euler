name := "project-euler"

version := "0.1.0"

scalaVersion := "2.10.1"

// fork in run := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:postfixOps")


resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
)




// vim: filetype=scala
