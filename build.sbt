name := "klearn"

version := "0.1"

scalaVersion := "2.13.8"

idePackagePrefix := Some("org.eter.klearn")
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
//libraryDependencies += "org.openjdk.jmh" % "jmh-core" % "1.32"
//libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.32"
//libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19"
//
//libraryDependencies += "pl.project13.scala" % "sbt-jmh" % "0.4.4"

//assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)
//
//assemblyJarName in assembly := "benchmarks.jar"
//
//mainClass in assembly := Some("org.openjdk.jmh.Main")
//
//fullClasspath in assembly += Attributed.blank(JavaTools)


//enablePlugins(JmhPlugin)
cancelable in Global := true
updateOptions := updateOptions.value.withCachedResolution(true)
