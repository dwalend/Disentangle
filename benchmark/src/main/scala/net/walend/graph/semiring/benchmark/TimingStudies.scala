package net.walend.graph.semiring.benchmark

import java.io.{PrintStream, FileOutputStream, File}

import scopt.OptionParser

/**
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudies {

  val studies: Map[String, TimingStudy] = Map("dijkstra"->DijkstraTiming,
                                              "jungDijkstra" -> JungDijkstraTiming,
                                              "floydWarshall" -> FloydWarshallTiming,
                                              "brandes" -> BrandesTiming)
  case class ArgsConfig(algorithm:TimingStudy = DijkstraTiming,nodeExponent:Int = 5, out:Option[File] = None)

  def main(args:Array[String]): Unit = {

    val argsParser = new OptionParser[ArgsConfig]("Disentangler Timing Studies"){
      head("sbt \"benchmark/run ...\"")

      opt[String]('a',"algorithm") action {(x,c) =>
        c.copy(algorithm = studies(x))} validate { x =>
          if(studies.contains(x)) success else failure(s"--algorithm must be one of ${studies.keySet.mkString(", ")}")
      } text (s"algorithm determines what algorithm to measure, one of ${studies.keySet.mkString(", ")}")

      opt[Int]('n', "nodeExponent") action { (x, c) =>
        c.copy(nodeExponent = x) } validate { x =>
          if (x >= 5) success else failure("--nodeExponent must be >= 5")
      } text ("nodeExponent defines the maximum number of nodes in the study via 2^nodeExponent.")

      opt[File]('o', "out") valueName("<file>") action { (x, c) =>
        c.copy(out = Some(x)) } text("out is the path to the output file")
    }

    val argsConfig: Option[ArgsConfig] = argsParser.parse(args,ArgsConfig())

    argsConfig.fold(){ argsConfig =>
      val results = argsConfig.algorithm.createResults(argsConfig.nodeExponent)
      val output = argsConfig.out.fold(System.out)(file => new PrintStream(new FileOutputStream(file)))

      output.println(formatOutput(results))

      output.flush()
    }
  }

  def formatOutput(results:Seq[(Int, Long, Long, Double)]):String = {

    val header:String = "nodes,measured (ns),expected (ns)"
    val columns:Seq[String] = results.map(x => s"${x._1},${x._2},${x._3}")

    val lines:Seq[String] = header +: columns
    lines.mkString("\n")
  }

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
