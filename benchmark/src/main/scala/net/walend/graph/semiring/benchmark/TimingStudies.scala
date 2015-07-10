package net.walend.graph.semiring.benchmark

/**
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudies {

  def main(args:Array[String]): Unit = {

    val floydResults = FloydWarshallTiming.createResults(7)

    val dijkstraResults = DijkstraTiming.createResults(7)
    println(formatOutput(dijkstraResults))

    val brandesResults = BrandesTiming.createResults(7)
    println(formatOutput(brandesResults))

  }

  val header:String = "nodes,measured (ns),expected (ns)"

  def formatOutput(results:Seq[(Int, Long, Long, Double)]):String = {

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
