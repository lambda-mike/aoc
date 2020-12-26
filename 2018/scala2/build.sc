import mill._, scalalib._

trait AoCModule extends ScalaModule {
  def scalaVersion = "2.13.2"

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.5")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object day01 extends AoCModule
object day03 extends AoCModule
object day11 extends AoCModule
