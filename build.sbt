name := "Graph4ScalaSemirings"

organization := "net.walend"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.8.0"

//todo 2.0
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

fork in test := true

javaOptions in test += "-Xmx3G"