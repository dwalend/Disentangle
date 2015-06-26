name := "ScalaGraphMinimizer"

organization := "net.walend"

// Project version. Only release version (w/o SNAPSHOT suffix) can be promoted.
version := "0.1.2"

isSnapshot := true

scalaVersion := "2.11.6"

sbtVersion := "0.13.8"

//todo remove crossScalaVersions := Seq("2.10.4",scalaVersion.value)

lazy val root = (project in file(".")).
  aggregate(graph, toScalaGraph, benchmark)

lazy val graph = project

lazy val toScalaGraph = project.dependsOn(graph)

lazy val benchmark = project.dependsOn(graph,
                                        toScalaGraph % "test->test;compile->compile")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

// To sync with Maven central, you need to supply the following information:
pomExtra := (
  <url>https://github.com/dwalend/ScalaGraphMinimizer</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
        <distribution>repo</distribution>
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
    </developers>)

///old bits

// Your project orgnization (package name)
//organization := "net.walend"
//todo organization := "net.walend.graph"

// Your profile name of the sonatype account. The default is the same with the organization
//profileName := "net.walend"
