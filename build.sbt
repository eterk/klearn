name := "klearn"

version := "0.1"

scalaVersion := "2.13.12"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value


cancelable in Global := true

updateOptions := updateOptions.value.withCachedResolution(true)
