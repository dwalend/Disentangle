package net.walend.graph.semiring.benchmark

/**
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudies {

  def main(args:Array[String]): Unit = {
    val floydResults = FloydWarshallTiming.createResults(7)
    floydResults.map(x => println(x))

    val dijkstraResults = DijkstraTiming.createResults(7)
    dijkstraResults.map(x => println(x))

    val brandesResults = BrandesTiming.createResults(7)
    brandesResults.map(x => println(x))

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
