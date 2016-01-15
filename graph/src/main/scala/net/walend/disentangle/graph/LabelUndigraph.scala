package net.walend.disentangle.graph

/**
  * A directed graph with labeled edges.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait LabelUndigraph[Node,Label] extends Undigraph[Node] {

  type OuterEdgeType = (NodePair[Node],Label)

  type InnerEdgeType = (NodePair[InnerNodeType],Label)

  /**
    * @return the label to return when no edge exists
    */
  def noEdgeExistsLabel:Label

  /**
    * @return the Label between a pair of nodes, or noEdgeExistsLabel if no edge exists.
    */
  def label(between:NodePair[InnerNodeType]):Label
}

/**
  * A digraph that exposes the indices of stored nodes.
  */
trait IndexedLabelUndigraph[Node,Label] extends LabelUndigraph[Node,Label] {

  /**
    * All the nodes in the graph, in an indexed set
    */
  def nodes:IndexedSet[Node]

  /**
    * @return internal representation of all of the nodes in the graph.
    */
  def innerNodes:IndexedSet[InnerNodeType]

  /**
    * An internal representation of nodes within the graph
    */
  trait InnerIndexedNodeTrait extends UndigraphInnerNodeTrait {

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

case class NodePair[A](_1: A, _2: A) {
  def contains(elem: A): Boolean =
    elem == _1 || elem == _2

  override def equals(other: Any): Boolean =
    other match {

      case that: NodePair[_] =>
        (that canEqual this) &&
          (((this._1 == that._1) &&
            (this._2 == that._2)) ||
          ((this._1 == that._2) &&
            (this._2 == that._1)))

      case _ => false
    }

  override def hashCode:Int = _1.hashCode + _2.hashCode

}