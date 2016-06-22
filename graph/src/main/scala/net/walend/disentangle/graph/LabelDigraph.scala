package net.walend.disentangle.graph

/**
 * A directed graph with labeled edges.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait LabelDigraph[Node,Label] extends Digraph[Node] {

  type OuterEdgeType = (Node,Node,Label)

  trait LabelDigraphEdgeTrait extends DigraphInnerEdgeTrait {
    def label:Label
  }

  override type InnerEdgeType <: LabelDigraphEdgeTrait

  /**
   * @return the label to return when no edge exists
   */
  //todo make this a function () => Label and throw a NoSuchElementException in 0.3 , to match LabelUndigraph
  def noEdgeExistsLabel:Label

  /**
   * @return the Edge between start and end or noEdgeExistsValue if no edge connects start to end
   */
  def label(start:InnerNodeType,end:InnerNodeType):Label

}

/**
  * A digraph that exposes the indices of stored nodes.
  *
  * @author dwalend
  * @since v0.1.0
  */
trait IndexedLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] with IndexedGraph[Node] {

  type InnerNodeType <: DigraphInnerNodeTrait with InnerIndexedNodeTrait

  /**
    * @return the label connecting nodes at index i and j, or noEdgeExistsLabel
    * @throws IndexOutOfBoundsException if either i or j does not correspond with a node
    */
  def label(i:Int,j:Int):Label

  //todo def edge(i:Int,j:Int):InnerEdgeType when it becomes clear what to do for edges that don't exist, and when the method is needed. (InnerNodeType,InnerNodeType,noEdgeExistsLabel) might be fine.

}