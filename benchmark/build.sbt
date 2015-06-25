scalaVersion := "2.11.6"

//todo remove crossScalaVersions := Seq("2.10.4",scalaVersion.value)

//todo move to a separate project and .jar
libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1" % "test" //for timing comparisons

libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.0.1" % "test" //for timing comparisons

libraryDependencies += "com.github.verbalexpressions" %% "ScalaVerbalExpression" % "1.0.1" % "test" //for loading the Enron graph

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.9.1" % "test" //for loading the Enron graph

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")