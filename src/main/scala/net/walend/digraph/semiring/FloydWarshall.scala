package net.walend.digraph.semiring

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FloydWarshall {

  def floydWarshall[Node,Label,Key](labelGraph:Digraph[Node,Label],support:SemiringSupport[Label,Key]):Digraph[Node,Label] = {
    val innerNodes = labelGraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      labelGraph.relax(support.semiring)(i,k,j)
    }
    labelGraph
  }

  def allPairsShortestPaths[Node,Label,Key](labelDigraph:Digraph[Node,Label],support:SemiringSupport[Label,Key]):Digraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }
}