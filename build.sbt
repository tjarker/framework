ThisBuild / scalaVersion := "3.5.1"

scalacOptions := Seq(
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls",
)

libraryDependencies += "net.java.dev.jna" % "jna" % "5.14.0"



libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.2.0"

