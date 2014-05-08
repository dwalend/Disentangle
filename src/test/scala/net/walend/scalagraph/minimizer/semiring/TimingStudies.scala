package net.walend.scalagraph.minimizer.semiring

import net.walend.scalagraph.minimizer.gengraph.GraphFactory

/**
 * @author dwalend
 * @since v0.0.1
 */
object TimingStudies {

  def main (args:Array[String]) {

    //Time the Floyd Warshall algorithm with AllShortestPaths
    val floydResults = study(8,timeFloyd,expectedTimeFloyd)

    floydResults.map(x => println(x))

    //Time Dijkstra's algorithm with AllShortestPaths
    val dijstraResults = study(8,timeDijkstra,expectedTimeDijkstra)

    dijstraResults.map(x => println(x))

    //Time Brandes' algorithm with AllShortestPaths
    val brandesResults = study(8,timeBrandes,expectedTimeDijkstra)

    brandesResults.map(x => println(x))

  }

  def timeBrandes(nodeCount:Int):Long = {
    val support:AllPaths[Int,Int,Int] = new AllPaths(FewestNodes)

    val semiring = support.semiring

    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val labelGraph = new AllShortestPathsGraphBuilder[Int](semiring).initialLabelGraph(graph)
    val result = timeFunction{Brandes.betweenness(support)(labelGraph)}

    result._2
  }

  def expectedTimeDijkstra(calibration:(Int,Long),nodeCount:Int):Long = {

    //O(|V|^2 ln|V|)
    def bigO(nodeCount:Int):Double = {
      Math.pow(nodeCount,2) * Math.log(nodeCount)
    }

    ((bigO(nodeCount)/bigO(calibration._1))*calibration._2).toLong
  }

  def timeDijkstra(nodeCount:Int):Long = {
    val support:AllPaths[Int,Int,Int] = new AllPaths(FewestNodes)

    val semiring = support.semiring

    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val labelGraph = new AllShortestPathsGraphBuilder[Int](semiring).initialLabelGraph(graph)
    val result = timeFunction{Dijkstra.allPairsShortestPaths(support)(labelGraph)}

    result._2
  }

  def expectedTimeFloyd(calibration:(Int,Long),nodeCount:Int):Long = {
    (Math.pow(nodeCount.toDouble/calibration._1,3) * calibration._2).toLong
  }

  def timeFloyd(nodeCount:Int):Long = {
    val support:AllPaths[Int,Int,Int] = new AllPaths(FewestNodes)

    val semiring = support.semiring

    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val labelGraph = new AllShortestPathsGraphBuilder[Int](semiring).initialLabelGraph(graph)
    val result = timeFunction{FloydWarshall.floydWarshall(semiring)(labelGraph)}

    result._2
  }

  def study(maxExponent:Int,timeF:Int => Long,expectedF:((Int,Long),Int) => Long):Seq[(Int,Long,Long)] = {

    warmUp(16,{timeF(64)})
    val nodeCountAndTime:Seq[(Int,Long)] = nodeCountsFrom32(maxExponent).map(x=>(x,timeF(x)))

    val calibration = nodeCountAndTime.head
    nodeCountAndTime.map(x => (x._1,x._2,expectedF(calibration,x._1)))
  }

  def nodeCountsFrom32(exponent:Int):Seq[Int] = {
    (5.0.to(exponent.toDouble,0.25)).map(x => Math.pow(2,x).toInt)
  }

  def warmUp[T](number:Int,body: ⇒ T) = {
    for(i <- 0 until number) body
  }

  def timeFunction[T](body: ⇒ T):(T,Long) = {
    val startTime:Long = System.nanoTime()
    val result = body
    val endTime:Long = System.nanoTime()
    (result,endTime-startTime)
  }
}
