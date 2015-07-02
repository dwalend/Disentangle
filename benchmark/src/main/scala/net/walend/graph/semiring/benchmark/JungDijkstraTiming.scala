package net.walend.graph.semiring.benchmark

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath
import edu.uci.ics.jung.graph.DirectedSparseGraph

import net.walend.graph.DigraphFactory

/**
 * @author dwalend
 * @since v0.0.1
 */
object JungDijkstraTiming {

  def main (args:Array[String]) {

    val maxExponent = if (args.size == 0) 7
    else java.lang.Integer.parseInt(args(1))

    //Time the algorithm with AllShortestPaths
    val results = createResults(maxExponent)
    results.map(x => println(x))
  }

  def createResults(maxExponent:Int) = {
    TimingStudy.study(maxExponent,timeJungDijkstra,DijkstraTiming.expectedTimeDijkstra)
  }


  def timeJungDijkstra(nodeCount: Int): Long = {

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount, 16)

    val jungGraph = new DirectedSparseGraph[Int, Any]()
    for (node <- graph.nodes) {
      jungGraph.addVertex(node)
    }

    var i = 0
    for (edge <- graph.edges) {
      jungGraph.addEdge(i, edge._1, edge._2)
      i = i + 1
    }

    val dijkstraShortestPath = new DijkstraShortestPath(jungGraph)
    val result = TimingStudy.timeFunction {
      import scala.collection.JavaConversions.iterableAsScalaIterable
      for (node <- iterableAsScalaIterable(jungGraph.getVertices)) {
        dijkstraShortestPath.getIncomingEdgeMap(node)
      }
    }

    result._2
  }
}
