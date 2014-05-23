package net.walend.digraph.semiring

import net.walend.digraph.MutableEdgeDigraph

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FloydWarshall {

  def relax[Node,Edge,Key](labelDigraph:MutableEdgeDigraph[Node,Edge],
                 semiring:SemiringSupport[Edge,Key]#Semiring)
                (from:labelDigraph.InnerNodeType,
                 through:labelDigraph.InnerNodeType,
                 to:labelDigraph.InnerNodeType):Edge = {
    val fromThrough:Edge = labelDigraph.edge(from,through)
    val throughTo:Edge = labelDigraph.edge(through,to)

    val current:Edge = labelDigraph.edge(from,to)

    val summaryLabel:Edge = semiring.relax(fromThrough,throughTo,current)

    labelDigraph.updateEdge(from,to,summaryLabel)

    summaryLabel
  }

  def floydWarshall[Node,Label,Key](labelGraph:MutableEdgeDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableEdgeDigraph[Node,Label] = {
    val innerNodes = labelGraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      relax(labelGraph,support.semiring)(i,k,j)
    }
    labelGraph
  }

  def allPairsShortestPaths[Node,Label,Key](labelDigraph:MutableEdgeDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableEdgeDigraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }
}