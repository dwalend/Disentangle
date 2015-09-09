package net.walend.graph.semiring.benchmark

import edu.uci.ics.jung.algorithms.shortestpath.DijkstraShortestPath
import edu.uci.ics.jung.graph.DirectedSparseGraph

import net.walend.graph.DigraphFactory

/**
 * @author dwalend
 * @since v0.0.1
 */
object JungDijkstraTiming extends Timeable {

  def measureTime(nodeCount: Int): Long = {

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

  def expectedTime(calibration:(Int,Long),nodeCount:Int):Long = {
    DijkstraTiming.expectedTime(calibration,nodeCount)
  }
}
