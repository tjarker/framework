ThisBuild / scalaVersion := "3.5.1"

scalacOptions := Seq(
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls",
)

fork := true
javaOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"


javacOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"

libraryDependencies += "net.java.dev.jna" % "jna" % "5.14.0"



libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

