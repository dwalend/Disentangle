package net.walend.disentangle.graph.semiring.benchmark

import java.io.File

import scopt.OptionParser

/**
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudies {

  val studies: Map[String, Timeable ] = Map(
    "dijkstra"->DijkstraTiming,
    "jungDijkstra" -> JungDijkstraTiming,
    "floydWarshall" -> FloydWarshallTiming,
    "brandes" -> BrandesTiming,
    "parDijkstra" -> ParDijkstraTiming,
//    "parBrandes" -> ParBrandesTiming
  )

  case class ArgsConfig(algorithm:Timeable = DijkstraTiming,lowExponent:Int = 5,highExponent:Int = 7, out:Option[File] = None) {
    def validate() = {
      require(lowExponent <= highExponent, s"--highExponent $highExponent must be greater than or equal to --lowExponent $lowExponent")
    }
  }

  def main(args:Array[String]): Unit = {

    val argsParser = new OptionParser[ArgsConfig]("Disentangler Timing Studies"){
      head("sbt \"benchmark/run ...\"")

      opt[String]('a',"algorithm") action {(x,c) =>
        c.copy(algorithm = studies(x))} validate { x =>
          if(studies.contains(x)) success else failure(s"--algorithm must be one of ${studies.keySet.mkString(", ")}")
      } text (s"algorithm determines what algorithm to measure, one of ${studies.keySet.mkString(", ")}")

      opt[Int]('l', "lowExponent") action { (x, c) =>
        c.copy(lowExponent = x) } validate { x =>
          if (x >= 5) success else failure("--lowExponent must be >= 5 for 32 nodes in the test.")
      } text ("lowExponent defines the lower number of nodes in the study via 2^lowExponent.")

      opt[Int]('h', "highExponent") action { (x, c) =>
        c.copy(highExponent = x)
      } text ("highExponent defines the maximum number of nodes in the study via 2^highExponent.")

      opt[File]('o', "out") valueName("<file>") action { (x, c) =>
        c.copy(out = Some(x)) } text("out is the path to the output file")


    }

    val argsConfig: Option[ArgsConfig] = argsParser.parse(args,ArgsConfig())

    argsConfig.fold(()){ argsConfig =>
//      val fileType = argsConfig.out.fold("csv")(file => file.getName.split('.').lastOption.fold("csv")(end => end))
      argsConfig.validate()

      val timingStudy = TimingStudy(argsConfig.algorithm.measureTime,
                                    argsConfig.algorithm.expectedTime,
                                    argsConfig.lowExponent,
                                    argsConfig.highExponent,
                                    argsConfig.out
                                    )

      timingStudy.study()

//      val output = argsConfig.out.fold(System.out)(file => new PrintStream(new FileOutputStream(file)))
/*
      val formatOutput:(Seq[(Int, Long, Long, Double)] => String) = fileType match {
        case s if s == "csv" => formatOutputCsv
        case s if s == "json" => formatOutputJson
        case _ => formatOutputCsv
      }
*/
//      output.println(formatOutput(results))

//      output.flush()
    }
  }

  /*
  def formatOutputCsv(results:Seq[(Int, Long, Long, Double)]):String = {

    //values are in nanoseconds
    val header:String = "nodes,measured,expected"
    val columns:Seq[String] = results.map(x => s"${x._1},${x._2},${x._3}")

    val lines:Seq[String] = header +: columns
    lines.mkString("\n")
  }
  */
/*
  def formatOutputJson(results:Seq[(Int, Long, Long, Double)]):String = {

    val nodes = ("nodes",results.map(x => x._1))
    //values are in nanoseconds
    val measured = ("mesaured",results.map(x => x._2))
    val expected = ("expected",results.map(x => x._3))

    import upickle.default._

    write((nodes,measured,expected))
  }
*/
  /*
    def expectedTimeSingleDijkstra(calibration:(Int,Long),nodeCount:Int):Long = {

      //O(|V| ln|V|)
      def bigO(nodeCount:Int):Double = {
        nodeCount * Math.log(nodeCount)
      }

      ((bigO(nodeCount)/bigO(calibration._1))*calibration._2).toLong
    }


    */
}
