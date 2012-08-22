name := "project-euler"

version := "0.1.0"

scalaVersion := "2.9.2"

fork in run := true

scalacOptions ++= Seq("-unchecked", "-deprecation")


resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.9"
)


// vim: filetype=scala
