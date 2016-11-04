libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1" % "test" //for timing comparisons

libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.0.1" % "test" //for timing comparisons

//libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.10.1" % "test" //for loading the Enron graph

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")
