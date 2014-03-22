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

  def floydWarshall[N,Label](labelGraph:MutableGraph[N,MLDiEdge])(semiring:Semiring[Label]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }

  import scala.language.higherKinds
  def allPairsShortestPaths[N:Manifest,
                            E[X] <: EdgeLikeIn[X],
                            Label]
                            (originalGraph:Graph[N,E])
                            (semiring:Semiring[Label])
                            (labelGraphBuilder:LabelGraphBuilder):Graph[N,MLDiEdge] = {
    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
}
