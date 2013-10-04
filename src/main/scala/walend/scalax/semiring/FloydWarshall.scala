package walend.scalax.semiring

import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v1
 */
object FloydWarshall {

  def floydWarshall[N,Label](labelGraph:MutableGraph[N,LDiEdge])(semiring:Semiring[Label]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }

  def allPairsShortestPaths[N:Manifest,Label](originalGraph:Graph[N,LDiEdge])(semiring:Semiring[Label])(labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {
    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
}
