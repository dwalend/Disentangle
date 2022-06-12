import mill._
import scalalib._
import coursier.maven.MavenRepository
import coursier.Repository
import mill.api.Loose
import mill.api.Result
import mill.define.{Command, Target}
import os.{CommandResult, Path}

object Shared {
  val scalacOptions = Seq("-deprecation")
  val scalaVersion = "2.12.16" //todo move to scala 2.13, then scala 3
  val javaVersion = "11.0.10" //todo new release?
}

object graph extends ScalaModule {
  override def artifactName: T[String] = "Disentangle-Graph"

  def scalaVersion = Shared.scalaVersion
  def javaVersion = Shared.javaVersion

  override def scalacOptions = Shared.scalacOptions

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"net.sf.jung:jung-graph-impl:2.0.1",
      ivy"net.sf.jung:jung-algorithms:2.0.1"
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