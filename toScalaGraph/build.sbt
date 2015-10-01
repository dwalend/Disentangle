scalaVersion := "2.11.7"

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")