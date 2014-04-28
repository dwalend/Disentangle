import SonatypeKeys._

name := "Graph4ScalaSemirings"

organization := "net.walend"

// Project version. Only release version (w/o SNAPSHOT suffix) can be promoted.
version := "0.0.0-SNAPSHOT"

//todo 2.11.0 ?
scalaVersion := "2.10.4"

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.8.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

fork in test := true

javaOptions in test += "-Xmx3G"

scalacOptions ++= Seq("-feature")

//old sbt cut-paste

//publishMavenStyle := true

//publishArtifact in Test := false

//pomIncludeRepository := { x => false }

// Import default settings. This changes `publishTo` settings to use the Sonatype repository and add several commands for publishing.
sonatypeSettings

// Your project orgnization (package name)
organization := "net.walend"
//todo organization := "net.walend.scalagraph"

// Your profile name of the sonatype account. The default is the same with the organization
profileName := "net.walend"

// To sync with Maven central, you need to supply the following information:
pomExtra := {
  <url>https://github.com/dwalend/ScalaGraphMinimizer</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com:dwalend/ScalaGraphMinimizer.git</connection>
      <url>github.com/dwalend/ScalaGraphMinimizer.git</url>
    </scm>
    <developers>
      <developer>
        <id>dwalend</id>
        <name>David Walend</name>
        <url>https://github.com/dwalend</url>
      </developer>
    </developers>
}

