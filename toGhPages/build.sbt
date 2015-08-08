scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

fork in run := true

javaOptions in run += "-Xmx3G" //prevents big GC

javaOptions in run += "-Xms3G" //prevents big GC

javaOptions in run += "-server" //does hotspot optimizations earlier

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.1",
  "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
)

jsDependencies += RuntimeDOM

jsDependencies += ProvidedJS / "algorithmTime.js"

