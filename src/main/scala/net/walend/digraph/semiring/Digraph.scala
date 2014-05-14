package net.walend.digraph.semiring

/**
 * Methods required for a Digraph representation for the algorithms.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait Digraph[Node,Edge] {

  /**
   * @return All the nodes in the graph
   */
  def nodes:Seq[Node]

  /**
   * An internal representation of nodes within the graph
   */
  trait InnerNodeTrait {
    def value:Node
    /* todo after FloydWarshall
    def successors:Seq[InnerNode]

    def predecessors:Seq[InnerNode]
    */
  }

  /**
   * The type of InnerNodeTrait for this digraph representation
   */
  type InnerNodeType <: InnerNodeTrait

  /**
   * @return InnerNode representation of all of the nodes in the graph.
   */
  def innerNodes:Seq[InnerNodeType]

  /**
   *
   * @param value a node that might be in this digraph
   * @return Some inner node if it exists in the digraph or None
   */
  def innerNode(value:Node):Option[InnerNodeType]

  /**
   * @return the value to return when no edge exists
   */
  def noEdgeExistsValue:Edge

  /**
   * @return All of the edges in the graph
   */
  def edges:Seq[(Node,Node,Edge)]

  /**
   * An edge that spans from start to end
   * @param start
   * @param end
   * @return the Edge between start and end or noEdgeExistsValue
   */
  def edge(start:InnerNodeType,end:InnerNodeType):Edge

  /**
   * Set the edge that spans from start to end to edge
   *
   * @param from
   * @param to
   * @param edge
   */
  def updateEdge(from:InnerNodeType,to:InnerNodeType,edge:Edge):Unit
  //todo toString, after FloydWarshall
}
