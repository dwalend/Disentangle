scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

//fork in run := true

//javaOptions in run += "-Xmx3G" //prevents big GC

//javaOptions in run += "-Xms3G" //prevents big GC

//javaOptions in run += "-server" //does hotspot optimizations earlier

//testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.1"//,
//  "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
)

//don't need phantomjs . //jsDependencies += RuntimeDOM

jsDependencies += "org.webjars" % "d3js" % "3.5.5-1" / "d3.min.js"

jsDependencies += ProvidedJS / "algorithmTime.js" //commonJSName "algorithmTime"

scalaJSStage in Global := FastOptStage

persistLauncher := true

//skip in packageJSDependencies := false