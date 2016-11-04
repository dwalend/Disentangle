libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.11.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")