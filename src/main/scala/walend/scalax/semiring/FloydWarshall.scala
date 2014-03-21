package walend.scalax.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v1
 */
object FloydWarshall {

  def floydWarshall[N,Label<:AnyRef](labelGraph:MutableGraph[N,MLDiEdge])(semiring:Semiring[Label]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }

  def allPairsShortestPaths[N:Manifest,Label<:AnyRef](originalGraph:Graph[N,MLDiEdge])(semiring:Semiring[Label])(labelGraphBuilder:LabelGraphBuilder):Graph[N,MLDiEdge] = {
    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
}
