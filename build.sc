import mill._
import scalalib._
import coursier.maven.MavenRepository
import coursier.Repository
import mill.api.Loose
import mill.api.Result
import mill.define.{Command, Target}
import mill.scalajslib.ScalaJSModule
import os.{CommandResult, Path}

object Shared {
  val scalacOptions = Seq("-deprecation")
  val scalaJSVersion = "1.13.0" //todo set up javascript again
  val scalaVersion = "3.2.2"
  val javaVersion = "17.0.6"
}

object TestTest extends ScalaJSModule { //ScalaModule {  //todo ScalaJSModule does not play nice with ScalaTest
  override def artifactName: T[String] = "Disentangle-TestTest"

  override def scalaJSVersion: T[String] = Shared.scalaJSVersion
  override def scalaVersion: T[String] = Shared.scalaVersion
  def javaVersion = Shared.javaVersion

  override def scalacOptions = Shared.scalacOptions

  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4"
  )

  object test extends Tests with TestModule.Munit {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::0.7.29"
      //      ivy"net.sf.jung:jung-graph-impl:2.1.1",
      //      ivy"net.sf.jung:jung-algorithms:2.1.1"
    )
  }
}

object Graph extends ScalaJSModule { //ScalaModule {  //todo ScalaJSModule does not play nice with ScalaTest
  override def artifactName: T[String] = "Disentangle-Graph"

  override def scalaJSVersion: T[String] = Shared.scalaJSVersion
  override def scalaVersion: T[String] = Shared.scalaVersion
  def javaVersion = Shared.javaVersion

  override def scalacOptions = Shared.scalacOptions

  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4"
  )

  object test extends Tests with TestModule.ScalaTest {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.15",
//      ivy"net.sf.jung:jung-graph-impl:2.1.1",
//      ivy"net.sf.jung:jung-algorithms:2.1.1"
    )
  }

  def millw(): Command[PathRef] = T.command {
    val target = mill.modules.Util.download("https://raw.githubusercontent.com/lefou/millw/main/millw")
    val millw = millSourcePath / "millw"
    os.copy.over(target.path, millw)
    os.perms.set(millw, os.perms(millw) + java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE)
    target
  }
}

//todo create a new module - GraphJvmTests - for the Jung tests - that can convert to Munit
//todo then move the Graph module to use Munit
//todo then build Graph as ScalaJS
//todo then move the rest to Munit

object GraphJvmTests extends ScalaModule {
  override def artifactName: T[String] = "Disentangle-Graph-JVM-Tests"

  override def scalaVersion: T[String] = Shared.scalaVersion

  def javaVersion = Shared.javaVersion

  override def scalacOptions = Shared.scalacOptions

  override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(Graph)

  object test extends Tests with TestModule.ScalaTest {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.15",
      ivy"net.sf.jung:jung-graph-impl:2.1.1",
      ivy"net.sf.jung:jung-algorithms:2.1.1"
    )
  }
}

object Examples extends ScalaModule {
  override def artifactName: T[String] = "Disentangle-Examples"

  def scalaJSVersion = Shared.scalaJSVersion
  override def scalaVersion: T[String] = Shared.scalaVersion
  def javaVersion = Shared.javaVersion

  override def scalacOptions: Target[Seq[String]] = Shared.scalacOptions

  override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(Graph)

  object test extends Tests with TestModule.ScalaTest {

    override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(Graph.test)

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.15"
    )
  }
}

object Benchmark extends ScalaModule {
  override def artifactName: T[String] = "Disentangle-Benchmark"

  def scalaJSVersion = Shared.scalaJSVersion
  override def scalaVersion: T[String] = Shared.scalaVersion
  def javaVersion = Shared.javaVersion

  override def scalacOptions = Shared.scalacOptions

  override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(Graph)

  override def ivyDeps = Agg(
    ivy"org.scalatest::scalatest:3.2.15",
    ivy"net.sf.jung:jung-graph-impl:2.1.1",
    ivy"net.sf.jung:jung-algorithms:2.1.1",
    ivy"com.github.scopt::scopt:4.1.0"
  )

  object test extends Tests with TestModule.ScalaTest {

    override def moduleDeps: Seq[JavaModule] = super.moduleDeps ++ Seq(Graph.test)

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.15"
    )
  }
}