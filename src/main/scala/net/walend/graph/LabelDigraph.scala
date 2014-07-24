package net.walend.graph

/**
 * A directed graph with labeled edges.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait LabelDigraph[Node,Label] extends Digraph[Node] {

  type OuterEdgeType = (Node,Node,Label)

  type InnerEdgeType = (InnerNodeType,InnerNodeType,Label)

  /**
   * @return the label to return when no edge exists
   */
  def noEdgeExistsLabel:Label

  /**
   * @return the Edge between start and end or noEdgeExistsValue if no edge connects start to end
   */
  def label(start:InnerNodeType,end:InnerNodeType):Label
}

/**
 * A graph where edges can be upserted.
 * 
 */
trait MutableLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] {

  /**
   * Set the edge that spans from start to end
   *
   */
  def upsertEdge(from:InnerNodeType,to:InnerNodeType,label:Label):Unit

}

/**
 * A digraph that exposes the indices of stored nodes.
 */
trait IndexedLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] {

  /**
   * All the nodes in the graph, in an indexed set
   */
  def nodes:IndexedSet[Node]

  /**
   * @return All the nodes in the graph in an indexed seq
   */
  @deprecated("replace with nodes")
  def nodesSeq:IndexedSeq[Node]

  /**
   * @return internal representation of all of the nodes in the graph.
   */
  def innerNodes:IndexedSet[InnerNodeType]

  /**
   * An internal representation of nodes within the graph
   */
  trait InnerIndexedNodeTrait extends DigraphInnerNodeTrait {

    def index:Int

  }

  /**
   * The type of InnerNodeTrait for this digraph representation
   */
  type InnerNodeType <: InnerIndexedNodeTrait

  def node(i:Int):Node

  def innerNodeForIndex(i:Int):InnerNodeType

  def label(i:Int,j:Int):Label
}