package net.walend.digraph

import scala.collection.GenTraversable

/**
 * A graph with Nodes that can be distinguished from each other. An InnerNodeType provides access to a nodes place in the graph
 *
 *
 * I've pulled definitions from Wikipedia: http://en.wikipedia.org/wiki/Graph_(mathematics) where possible
 *
 * @author dwalend
 * @since v0.1.0
 */
//todo split up and start a package.scala
trait Graph[Node] {

  def nodes:Seq[Node]

  /**
   * An internal representation of nodes within the graph
   */
  trait InnerNodeTrait {
    def value:Node

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
   * @param value a node that might be in this digraph
   * @return Some inner node if it exists in the digraph or None
   */
  def innerNode(value:Node):Option[InnerNodeType]

  type OuterEdgeType

  type InnerEdgeType

  /**
   * @return A Traversable (usually something more specific) of the edges
   */
  def edges:GenTraversable[OuterEdgeType]

  /**
   * @return A Traversable of the edges as represented in the graph
   */
  def innerEdges:GenTraversable[InnerEdgeType]
}


/**
 * A graph with directed zero or one label from any single node to any other single node.
 * 
 */
//todo any way to pass InnerNodeType up as part of Edge?
trait LabelDigraph[Node,Label] extends Graph[Node] {

  /**
   * An internal representation of nodes within the graph
   */
  trait DigraphInnerNodeTrait extends InnerNodeTrait {
    def value:Node

    def successors:Seq[(InnerNodeType,InnerNodeType,Label)]

    def predecessors:Seq[(InnerNodeType,InnerNodeType,Label)]

  }

  /**
   * The type of InnerNodeTrait for this digraph representation
   */
  type InnerNodeType <: DigraphInnerNodeTrait

  type OuterEdgeType = (Node,Node,Label)

  /**
   * @return the label to return when no arc exists
   */
  def noArcExistsLabel:Label

  /**
   * @return All of the arcs in the graph
   */
  def arcs:GenTraversable[OuterEdgeType]

  def edges:GenTraversable[OuterEdgeType] = arcs

  /**
   * @return the Arc between start and end or noArcExistsValue
   */
  def label(start:InnerNodeType,end:InnerNodeType):Label

}

/**
 * A graph where arcs can be upserted.
 * 
 */
trait MutableLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] {

  /**
   * Set the arc that spans from start to end
   *
   */
  def upsertArc(from:InnerNodeType,to:InnerNodeType,label:Label):Unit

}

/**
 * A digraph that exposes the indices of stored nodes.
 */
trait IndexedLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] {

  /**
   * @return All the nodes in the graph
   */
  def nodes:IndexedSeq[Node]

  /**
   * @return internal representation of all of the nodes in the graph.
   */
  def innerNodes:IndexedSeq[InnerNodeType]

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

  //todo remove after changing the output from Dijkstra's algorithm
  def node(i:Int):Node

  def innerNodeForIndex(i:Int):InnerNodeType

  def label(i:Int,j:Int):Label
}