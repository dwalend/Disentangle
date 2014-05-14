package net.walend.digraph.semiring

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FloydWarshall {

  def floydWarshall[Node,Label,Key](labelGraph:Digraph[Node,Label],support:GraphMinimizerSupport[Label,Key]):Digraph[Node,Label] = {
    val innerNodes = labelGraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      support.semiring.relax(labelGraph)(i,k,j)
    }
    labelGraph
  }

  def allPairsShortestPaths[Node,Edge,Label,Key](digraph:Digraph[Node,Edge],graphConverter:Digraph[Node,Edge]=>Digraph[Node,Label],support:GraphMinimizerSupport[Label,Key]):Digraph[Node,Label] = {

    val labelDigraph = graphConverter(digraph)

    floydWarshall(labelDigraph,support)
  }
}