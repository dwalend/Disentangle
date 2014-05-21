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

    def successors:Seq[InnerNodeType]

    def predecessors:Seq[InnerNodeType]

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
   * @return the Edge between start and end or noEdgeExistsValue
   */
  def edge(start:InnerNodeType,end:InnerNodeType):Edge

  /**
   * Set the edge that spans from start to end to edge
   *
   */
  def updateEdge(from:InnerNodeType,to:InnerNodeType,edge:Edge):Unit

  //todo then make relax for other representations -- ordered lists -- and use them in Dijkstra's algorithm
  def relax[Key](semiring:SemiringSupport[Edge,Key]#Semiring)
                 (from:InnerNodeType,
                  through:InnerNodeType,
                  to:InnerNodeType):Edge = {

    val fromThrough:Edge = edge(from,through)
    val throughTo:Edge = edge(through,to)
    val fromThroughTo:Edge = semiring.extend(fromThrough,throughTo)

    val current:Edge = edge(from,to)

    val summaryLabel:Edge = semiring.summary(fromThroughTo,current)

    updateEdge(from,to,summaryLabel)

    summaryLabel
  }

}

/**
 * A digraph that exposes the indices of stored nodes.
 */
trait IndexedDigraph[Node,Edge] extends Digraph[Node,Edge] {

  /**
   * @return All the nodes in the graph, now indexed
   */
  def nodes:IndexedSeq[Node]

  /**
   * @return InnerNode representation of all of the nodes in the graph.
   */
  def innerNodes:IndexedSeq[InnerNodeType]

  /**
   * An internal representation of nodes within the graph
   */
  trait InnerIndexedNodeTrait extends InnerNodeTrait {

    def index:Int

  }

  /**
   * The type of InnerNodeTrait for this digraph representation
   */
  type InnerNodeType <: InnerIndexedNodeTrait

  def node(i:Int):Node

  def innerNodeForIndex(i:Int):InnerNodeType

  def edge(i:Int,j:Int):Edge
}