scalaVersion := "2.11.6"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1"  //for timing comparisons

libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.0.1"  //for timing comparisons

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

fork in run := true

javaOptions in run += "-Xmx3G" //prevents big GC

javaOptions in run += "-Xms3G" //prevents big GC

javaOptions in run += "-server" //does hotspot optimizations earlier
