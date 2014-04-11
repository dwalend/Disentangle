name := "Graph4ScalaSemirings"

organization := "net.walend"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.8.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

fork in test := true

javaOptions in test += "-Xmx3G"