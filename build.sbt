name := "Disentangle"

//organization in ThisBuild := "net.walend.disentangle"
organization in ThisBuild := "net.walend.disentangle"

// Project version. Only release version (w/o SNAPSHOT suffix) can be promoted.
version := "0.2.1-SNAPSHOT"

isSnapshot := false

scalaVersion := "2.11.8"

sbtVersion := "0.13.9"

lazy val root: Project = Project(
  id        = "root",
  base      = file("."),
  aggregate = Seq(graph, toScalaGraph, benchmark, examples),
  settings  = Project.defaultSettings ++ Seq(
    packagedArtifacts := Map.empty           // prevent publishing superproject artifacts
  )
)
.settings(unidocSettings: _*)


lazy val graph = project

lazy val toScalaGraph = project.dependsOn(graph)

lazy val benchmark = project.dependsOn(graph,toScalaGraph % "test->test;compile->compile")

lazy val examples = project.dependsOn(graph % "test->test;compile->compile")

//git.remoteRepo := "git@github.com:dwalend/disentangle.git"

publishMavenStyle := true


publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

//pomIncludeRepository := { _ => false }  //only needed if there are dependencies outside Sonatype Nexus

// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName in ThisBuild := "net.walend"

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/dwalend/disentangler"))

// To sync with Maven central, you need to supply the following information:
pomExtra in Global := {
  <url>https://github.com/dwalend/Disentangle</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com:dwalend/Disentangle.git</connection>
      <url>github.com/dwalend/Disentangle.git</url>
      <developerConnection>scm:git:git@github.com:dwalend/Disentangle.git</developerConnection>
    </scm>
    <developers>
      <developer>
        <id>dwalend</id>
        <name>David Walend</name>
        <url>https://github.com/dwalend</url>
      </developer>
    </developers>
}