package walend.scalax.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v1
 */
object FloydWarshall {

  def floydWarshall[N,Label](semiring:Semiring[Label])(labelGraph:MutableGraph[N,MLDiEdge]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }

  import scala.language.higherKinds
  def allPairsShortestPaths[N:Manifest,
                            E[X] <: EdgeLikeIn[X],
                            Label]
                            (semiring:Semiring[Label],labelGraphBuilder:LabelGraphBuilder)
                            (originalGraph:Graph[N,E]):Graph[N,MLDiEdge] = {
    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(semiring)(originalGraph)
    floydWarshall(semiring)(labelGraph)
    labelGraph
  }
}
