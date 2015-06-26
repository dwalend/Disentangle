scalaVersion := "2.11.6"

//todo remove crossScalaVersions := Seq("2.10.4",scalaVersion.value)

//todo move to a separate project and .jar
libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")