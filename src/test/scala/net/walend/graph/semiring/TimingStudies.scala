package net.walend.graph.semiring

import net.walend.scalagraph.semiring.GraphFactory
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * @author dwalend
 * @since v0.0.1
 */
object TimingStudies {

  def main (args:Array[String]) {

    //Time the Floyd Warshall algorithm with AllShortestPaths
//    val floydResults = study(8,timeFloyd,expectedTimeFloyd)
//    floydResults.map(x => println(x))

    //Time Dijkstra's algorithm with AllShortestPaths
//    val dijkstraResults = study(11,timeDijkstra,expectedTimeDijkstra)
//    dijkstraResults.map(x => println(x))

//    val scalaGraphConvertDijstraResults = study(10,timeScalaGraphConvertDijkstra,expectedTimeDijkstra)
//    scalaGraphConvertDijstraResults.map(x => println(x))

    /*
        val jungDijkstraResults = study(10,timeJungDijkstra,expectedTimeDijkstra)
        jungDijkstraResults.map(x => println(x))

        val scalaGraphDijkstraMap = scalaGraphDijstraResults.map(x => (x._1,(x._2,x._3))).toMap
        val jungDijkstraMap = jungDijkstraResults.map(x => (x._1,(x._2,x._3))).toMap
        val dijkstraMap = dijkstraResults.map(x => (x._1,(x._2,x._3))).toMap
        val compareResults = dijkstraMap.keys.map(x => (x,(scalaGraphDijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1),(dijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1))).toSeq.sortBy(_._1)
    //    val compareResults = dijkstraMap.keys.map(x => (x,(dijkstraMap(x)._1.toDouble / jungDijkstraMap(x)._1))).toSeq.sortBy(_._1)

        compareResults.map(x => println(x))
    */

    //Time Brandes' algorithm with AllShortestPaths
    val brandesResults = study(11,timeBrandes,expectedTimeDijkstra)
    brandesResults.map(x => println(x))
  }

  def timeFloyd(nodeCount:Int):Long = {

    import net.walend.graph.DigraphFactory
    import net.walend.graph.semiring.AllPathsFirstSteps
    import net.walend.graph.semiring.{FewestNodes => FFewestNodes}

    val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)
    //    val support = FFewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = timeFunction{FloydWarshall.allPairsShortestPaths(graph.edges,graph.nodesSeq,support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))}
    /*
        val result = timeFunction{
            val initNode = initialGraph.innerNodes.head
            DDijkstra.dijkstraSingleSource(initialGraph, support)(initNode)
        }
    */

    //    println(s"$nodeCount ${result._2}")

    result._2
  }

  def timeDijkstra(nodeCount:Int):Long = {

    import net.walend.graph.DigraphFactory
    import net.walend.graph.semiring.{Dijkstra => DDijkstra}
    import net.walend.graph.semiring.AllPathsFirstSteps
    import net.walend.graph.semiring.{FewestNodes => FFewestNodes}

    val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)
//    val support = FFewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = timeFunction{DDijkstra.allPairsShortestPaths(graph.edges,graph.nodesSeq,support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))}
/*
    val result = timeFunction{
        val initNode = initialGraph.innerNodes.head
        DDijkstra.dijkstraSingleSource(initialGraph, support)(initNode)
    }
*/

    //    println(s"$nodeCount ${result._2}")

    result._2
  }

  def timeBrandes(nodeCount:Int):Long = {

    import net.walend.graph.DigraphFactory

    val support = FewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = timeFunction{Brandes.allLeastPathsAndBetweenness(graph.edges,graph.nodesSeq,support,FewestNodes.convertEdgeToLabel)}

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

  def timeScalaGraphConvertDijkstra(nodeCount:Int):Long = {

    import net.walend.graph.semiring.{FewestNodes => FFewestNodes}
    import net.walend.graph.semiring.{Dijkstra => DDijkstra}
    import net.walend.graph.semiring.AllPathsFirstSteps
    import scalax.collection.Graph
    import scalax.collection.GraphEdge.DiEdge
    import net.walend.scalagraph.semiring.ConvertToLabelDigraph

    val support:AllPathsFirstSteps[Int,Int,Int] = new AllPathsFirstSteps(FFewestNodes)

    val graph:Graph[Int,DiEdge] = GraphFactory.createRandomNormalGraph(nodeCount,16)

    import scala.language.higherKinds
    def convertToLabel[E[X] <: EdgeLikeIn[X]](edge:E[Int]):(Int,Int,Option[FirstStepsTrait[Int,Int]]) = {
      (edge._1,edge._2,Some(support.FirstSteps(1,Set.empty[Int])))
    }

    val result = timeFunction{
      val labelGraphParts = ConvertToLabelDigraph.convert(graph,support)(convertToLabel)

      def labelForLabel[N,E,L](from:N,to:N,edge:E):L = edge.asInstanceOf[L]
      DDijkstra.allPairsShortestPaths(labelGraphParts._1,labelGraphParts._2,support,labelForLabel)
    }

    result._2
  }

  def expectedTimeFloyd(calibration:(Int,Long),nodeCount:Int):Long = {
    (Math.pow(nodeCount.toDouble/calibration._1,3) * calibration._2).toLong
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
