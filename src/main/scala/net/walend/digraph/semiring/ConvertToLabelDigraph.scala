package net.walend.digraph.semiring

/**
 * Converts Digraphs to Digraphs of Labels for semiring algorithms. 
 * 
 * The most general case for this is 
 * 
 * def createLabelGraph[Node,Label](body: ⇒ Digraph[Node,Label]):Digraph[Node,Label]
 * 
 * but that's not terribly useful.
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
                                   labelForEdge:(Node,Node,Edge)=>Label):IndexedDigraph[Node,Label] = {

    val nodes = digraph.nodes
    val edges = digraph.nodes.map(x => (x,x,support.semiring.I)) ++
                digraph.edges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    FastDigraph(edges,nodes,support.semiring.O)
  }
}