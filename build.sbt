import SonatypeKeys._

// Import default settings. This changes `publishTo` settings to use the Sonatype repository and add several commands for publishing.
sonatypeSettings

//todo maybe remove release
releaseSettings

name := "ScalaGraphMinimizer"

organization := "net.walend"

// Project version. Only release version (w/o SNAPSHOT suffix) can be promoted.
version := "0.1.1"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4",scalaVersion.value)

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1" % "test" //for timing comparisons

libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.0.1" % "test" //for timing comparisons

resolvers += "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/"

//no 2.10 support

libraryDependencies += "com.github.verbalexpressions" %% "ScalaVerbalExpression" % "1.0.1" % "test" //for loading the Enron graph

fork in test := true

javaOptions in test += "-Xmx3G" //prevents big GC

javaOptions in test += "-Xms3G" //prevents big GC

javaOptions in test += "-server" //does hotspot optimizations earlier

fork in run := true

javaOptions in run += "-Xmx3G" //prevents big GC

javaOptions in run += "-Xms3G" //prevents big GC

javaOptions in run += "-server" //does hotspot optimizations earlier

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

// Your project orgnization (package name)
organization := "net.walend"
//todo organization := "net.walend.graph"

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
