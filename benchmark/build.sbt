libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1"  //for timing comparisons

libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.0.1"  //for timing comparisons

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0" //command line parser

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.3.4" //json output

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

fork in run := true

javaOptions in run += "-Xmx3G" //prevents big GC

javaOptions in run += "-Xms3G" //prevents big GC

javaOptions in run += "-server" //does hotspot optimizations earlier

//javaOptions in run += "-Dscala.concurrent.context.numThreads=4"

packagedArtifacts := Map.empty
