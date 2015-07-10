package net.walend.graph.semiring.benchmark

import java.io.{PrintStream, FileOutputStream, File}

import scopt.OptionParser

/**
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudies {

  case class ArgsConfig(nodeExponent:Int = 5, out:Option[File] = None)

  def main(args:Array[String]): Unit = {

    val argsParser = new OptionParser[ArgsConfig]("Disentangler Timing Studies"){
      head("sbt \"benchmark/runMain net.walend.graph.semiring.benchmark.TimingStudies\"")
      opt[Int]('n', "nodeExponent") action { (x, c) =>
        c.copy(nodeExponent = x) } text("nodeExponent defines the maximum number of nodes in the study via 2^nodeExponent.")
      opt[File]('o', "out") valueName("<file>") action { (x, c) =>
        c.copy(out = Some(x)) } text("out is the path to the output file")
    }

    val argsConfig: Option[ArgsConfig] = argsParser.parse(args,ArgsConfig())

    argsConfig.fold(){ argsConfig =>
      val dijkstraResults = DijkstraTiming.createResults(argsConfig.nodeExponent)
      val output = argsConfig.out.fold(System.out)(file => new PrintStream(new FileOutputStream(file)))

      output.println(formatOutput(dijkstraResults))

      output.flush()
    }

//    val floydResults = FloydWarshallTiming.createResults(7)


//    val brandesResults = BrandesTiming.createResults(7)
//    println(formatOutput(brandesResults))

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
