package net.walend.scalagraph.minimizer.semiring

import net.walend.scalagraph.minimizer.gengraph.GraphFactory
import net.walend.digraph.{FastDigraph, IndexedDigraph}

/**
 * @author dwalend
 * @since v0.0.1
 */
object TimingStudies {

  def main (args:Array[String]) {
    /*
    //Time the Floyd Warshall algorithm with AllShortestPaths
    val floydResults = study(8,timeFloyd,expectedTimeFloyd)
    floydResults.map(x => println(x))
     */
    //Time Dijkstra's algorithm with AllShortestPaths
//    val scalaGraphDijstraResults = study(9,timeScalaGraphDijkstra,expectedTimeDijkstra)
//    scalaGraphDijstraResults.map(x => println(x))


    val jungDijkstraResults = study(10,timeJungDijkstra,expectedTimeDijkstra)
    jungDijkstraResults.map(x => println(x))

    val dijkstraResults = study(10,timeDijkstra,expectedTimeDijkstra)
//    val dijkstraResults = study(12,timeDijkstra,expectedTimeSingleDijkstra)
    dijkstraResults.map(x => println(x))


//    val scalaGraphDijkstraMap = scalaGraphDijstraResults.map(x => (x._1,(x._2,x._3))).toMap
    val jungDijkstraMap = jungDijkstraResults.map(x => (x._1,(x._2,x._3))).toMap
    val dijkstraMap = dijkstraResults.map(x => (x._1,(x._2,x._3))).toMap
//    val compareResults = dijkstraMap.keys.map(x => (x,(scalaGraphDijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1),(dijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1))).toSeq.sortBy(_._1)
    val compareResults = dijkstraMap.keys.map(x => (x,(dijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1))).toSeq.sortBy(_._1)
    compareResults.map(x => println(x))

    /*
    //Time Brandes' algorithm with AllShortestPaths
    val brandesResults = study(8,timeBrandes,expectedTimeDijkstra)
    brandesResults.map(x => println(x))
    */

  }

  def timeDijkstra(nodeCount:Int):Long = {

    import net.walend.digraph.DigraphFactory
    import net.walend.digraph.semiring.{Dijkstra => DDijkstra}
    import net.walend.digraph.semiring.AllPathsFirstSteps
    import net.walend.digraph.semiring.{FewestNodes => FFewestNodes}
    import net.walend.digraph.semiring.ConvertToLabelDigraph
    import net.walend.digraph.Digraph

    val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)
//    val support = FFewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val initialGraph:IndexedDigraph[Int,support.Label] = ConvertToLabelDigraph.convert(graph,support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))
//    val initialGraph:IndexedDigraph[Int,support.Label] = ConvertToLabelDigraph.convert(graph,support,FFewestNodes.convertEdgeToLabel)

    val result = timeFunction{DDijkstra.allPairsShortestPaths(initialGraph,support)}
/*
    val result = timeFunction{
        val initNode = initialGraph.innerNodes.head
        DDijkstra.dijkstraSingleSource(initialGraph, support)(initNode)
    }
*/

    //    println(s"$nodeCount ${result._2}")

    result._2
  }

  def timeJungDijkstra(nodeCount:Int):Long = {

    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)

    import edu.uci.ics.jung.graph.DirectedSparseGraph
    import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath
    import scala.collection.JavaConversions._

    val jungGraph = new DirectedSparseGraph[Int,Any]()
    for(node <- graph.nodes) {
      jungGraph.addVertex(node)
    }

    var i=0
    for(edge <- graph.edges) {
      jungGraph.addEdge(i,edge._1,edge._2)
      i = i + 1
    }

    val dijkstraShortestPath = new DijkstraShortestPath(jungGraph)
    val result = timeFunction{for(node <- jungGraph.getVertices){
      dijkstraShortestPath.getIncomingEdgeMap(node)
    }}

    result._2
  }

  def timeScalaGraphBrandes(nodeCount:Int):Long = {
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

  def expectedTimeSingleDijkstra(calibration:(Int,Long),nodeCount:Int):Long = {

    //O(|V| ln|V|)
    def bigO(nodeCount:Int):Double = {
      nodeCount * Math.log(nodeCount)
    }

    ((bigO(nodeCount)/bigO(calibration._1))*calibration._2).toLong
  }

  def timeScalaGraphDijkstra(nodeCount:Int):Long = {
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

  def timeScalaGraphFloyd(nodeCount:Int):Long = {
    val support:AllPaths[Int,Int,Int] = new AllPaths(FewestNodes)

    val semiring = support.semiring

    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val labelGraph = new AllShortestPathsGraphBuilder[Int](semiring).initialLabelGraph(graph)
    val result = timeFunction{FloydWarshall.floydWarshall(semiring)(labelGraph)}

    result._2
  }

  def study(maxExponent:Int,timeF:Int => Long,expectedF:((Int,Long),Int) => Long):Seq[(Int,Long,Long,Double)] = {

    warmUp(16,{timeF(32)})
    warmUp(16,{timeF(64)})
    warmUp(16,{timeF(128)})
    val nodeCountAndTime:Seq[(Int,Long)] = nodeCountsFrom32(maxExponent).map(x=>(x,timeF(x)))

    val calibration = nodeCountAndTime.head
    val expected = nodeCountAndTime.map(x => x._1 -> expectedF(calibration,x._1)).toMap
    val ratio = nodeCountAndTime.map(x => x._1 -> x._2.toDouble/expected.get(x._1).get).toMap

    nodeCountAndTime.map(x => (x._1,x._2,expected(x._1),ratio(x._1)))
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
