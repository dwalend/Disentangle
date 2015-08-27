scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

resolvers += Resolver.sonatypeRepo("snapshots") //until goggles makes a real release

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.1",
  "org.scala-js" %%% "scalajs-tools" % "0.6.4",
  "com.lihaoyi" %%% "upickle" % "0.3.4",
  "com.yakticus" %%% "goggles" % "0.1-SNAPSHOT"
)

jsDependencies += "org.webjars" % "d3js" % "3.5.5-1" / "d3.min.js" //todo maybe you don't need this

jsDependencies += ProvidedJS / "algorithmTime.js" //commonJSName "algorithmTime"

scalaJSStage in Global := FastOptStage //nodejs

//jsDependencies += "org.webjars.npm" % "jsdom" % "5.4.3" / "jsdom.js"

//jsDependencies += RuntimeDOM  //phantomjs .

//libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"


//persistLauncher := true

//skip in packageJSDependencies := false

//testFrameworks += new TestFramework("utest.runner.Framework")
