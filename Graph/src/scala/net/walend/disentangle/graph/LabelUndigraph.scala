package net.walend.disentangle.graph

/**
  * A directed graph with labeled edges.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait LabelUndigraph[Node,Label] extends Undigraph[Node] {

  type OuterEdgeType = (NodePair[Node],Label)

  /**
    * @return the label to return when no edge exists
    */
  def noEdgeExistsLabel:Label

  /**
    * @return the Label between a pair of nodes, or noEdgeExistsLabel if no edge exists.
    */
  def label(between:NodePair[InnerNodeType]):Label

  /**
    * @return the Label between a pair of nodes, or noEdgeExistsLable if no edge exists.
    * @throws IllegalArgumentException if either node is not in the graph
    */

  def edge(between:NodePair[Node]):InnerEdgeType
}

/**
  * A digraph that exposes the indices of stored nodes.
  */
trait IndexedLabelUndigraph[Node,Label] extends  IndexedGraph[Node] with LabelUndigraph[Node,Label] {

  type InnerNodeType <: UndigraphInnerNodeTrait with InnerIndexedNodeTrait

  /**
    * @return the label connecting edge i to edge j, or noEdgeExists
    */
  def label(i:Int,j:Int):Label

  //todo def edge(i:Int,j:Int):InnerEdgeType when needed

}