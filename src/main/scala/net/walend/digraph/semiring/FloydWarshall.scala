package net.walend.digraph.semiring

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FloydWarshall {

  def floydWarshall[Node,Label](labelGraph:Digraph[Node,Label],semiring:Semiring[Label]):Digraph[Node,Label] = {
    val innerNodes = labelGraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      semiring.relax(labelGraph)(i,k,j)
    }
    labelGraph
  }

  def allPairsShortestPaths[Node,Edge,Label](digraph:Digraph[Node,Edge],graphConverter:Digraph[Node,Edge]=>Digraph[Node,Label],semiring:Semiring[Label]):Digraph[Node,Label] = {
    val labelDigraph = graphConverter(digraph)

    floydWarshall(labelDigraph,semiring)
  }
}