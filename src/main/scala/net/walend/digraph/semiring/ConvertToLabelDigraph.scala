package net.walend.digraph.semiring

import net.walend.digraph.{MatrixAndAdjacencyDigraph, Digraph}

/**
 * Converts Digraphs to Digraphs of Labels for semiring algorithms. 
 *
 * @author dwalend
 * @since v0.1.0
 */
object ConvertToLabelDigraph {

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   * 
   * @return a FastDigraph with graph's nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def convert[Node,Edge,Label,Key](digraph:Digraph[Node,Edge],
                                   support:SemiringSupport[Label,Key],
                                   labelForEdge:(Node,Node,Edge)=>Label):MatrixAndAdjacencyDigraph[Node,Label] = {

    val nodes = digraph.nodes
    val edges = digraph.nodes.map(x => (x,x,support.semiring.I)) ++
                digraph.edges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    MatrixAndAdjacencyDigraph(edges,nodes,support.semiring.O)
  }
}