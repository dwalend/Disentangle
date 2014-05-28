package net.walend.digraph.semiring

import net.walend.digraph.{Digraph, MutableEdgeDigraph}

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


    summaryLabel
  }

  def floydWarshall[Node,Label,Key](labelDigraph:MutableEdgeDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableEdgeDigraph[Node,Label] = {
    val innerNodes = labelDigraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      val summaryLabel = relax(labelDigraph,support.semiring)(i,k,j)
      labelDigraph.updateEdge(i,j,summaryLabel)
    }
    labelDigraph
  }

  def allPairsShortestPaths[Node,Label,Key](labelDigraph:MutableEdgeDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableEdgeDigraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   *
   * @return a FastDigraph with graph's nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def convert[Node,Edge,Label,Key](digraph:Digraph[Node,Edge],
                                   support:SemiringSupport[Label,Key],
                                   labelForEdge:(Node,Node,Edge)=>Label):MutableEdgeDigraph[Node,Label] = {
    val nodes = digraph.nodes
    val nonSelfEdges = digraph.edges.filter(x => x._1 != x._2)
    val edges = digraph.nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfEdges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    //todo just hand back a pure matrix Digraph instead
    import net.walend.digraph.MatrixDigraph
    MatrixDigraph(edges,nodes,support.semiring.O)
  }

}