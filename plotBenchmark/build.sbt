scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.1",
  "org.scala-js" %%% "scalajs-tools" % "0.6.4",
  "com.lihaoyi" %%% "upickle" % "0.3.4"
)

jsDependencies += "org.webjars" % "d3js" % "3.5.5-1" / "d3.min.js"

jsDependencies += ProvidedJS / "algorithmTime.js" //commonJSName "algorithmTime"

scalaJSStage in Global := FastOptStage

//don't need phantomjs .

//jsDependencies += RuntimeDOM

//libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"

//jsDependencies += "org.webjars.npm" % "jsdom" % "5.4.3" / "jsdom.js"

//persistLauncher := true

//skip in packageJSDependencies := false

//fork in run := true

//javaOptions in run += "-Xmx3G" //prevents big GC

//javaOptions in run += "-Xms3G" //prevents big GC

//javaOptions in run += "-server" //does hotspot optimizations earlier

//testFrameworks += new TestFramework("utest.runner.Framework")
