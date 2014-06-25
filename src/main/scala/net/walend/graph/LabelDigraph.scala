package net.walend.graph

import scala.collection.{GenSeq, GenTraversable}

/**
 * A graph with Nodes that can be distinguished from each other. An InnerNodeType provides access to a nodes place in the graph
 *
 *
 * I've pulled definitions from Wikipedia: http://en.wikipedia.org/wiki/Graph_(mathematics) where possible
 *
 * @author dwalend
 * @since v0.1.0
 */
//todo split up
trait Graph[Node] {

  //todo this should really be a GenSet, but subtraits need a Set with a fixed, controlled order, accessible by index
  def nodes:GenSeq[Node]

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
 * A graph with directed zero or one edge from any single node to any other single node.
 *
 */
trait Digraph[Node] extends Graph[Node] {

  trait DigraphInnerNodeTrait extends InnerNodeTrait {
    def value:Node

    def successors:Seq[InnerEdgeType]

    def predecessors:Seq[InnerEdgeType]
  }

  /**
   * The type of InnerNodeTrait for this digraph representation
   */
  type InnerNodeType <: DigraphInnerNodeTrait

}

/**
 * A directed graph with labeled edges.
 * 
 */
trait LabelDigraph[Node,Label] extends Digraph[Node] {

  type OuterEdgeType = (Node,Node,Label)

  type InnerEdgeType = (InnerNodeType,InnerNodeType,Label)

  /**
   * @return the label to return when no edge exists
   */
  def noEdgeExistsLabel:Label

  /**
   * @return the Edge between start and end or noEdgeExistsValue
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