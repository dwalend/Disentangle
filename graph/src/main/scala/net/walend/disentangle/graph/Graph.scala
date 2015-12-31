package net.walend.disentangle.graph

import scala.collection.{GenSet, GenTraversable}

/**
 * Ancestor trait for a variety of Graphs.
 *
 * A graph with Nodes that can be distinguished from each other. An InnerNodeType provides access to a nodes place in the graph.
 *
 * I've pulled definitions from Wikipedia: http://en.wikipedia.org/wiki/Graph_(mathematics) where possible
 *
 * @author dwalend
 * @since v0.1.0
 */
trait Graph[Node] {

  /**
   * All the nodes in the graph
   */
  def nodes:GenSet[Node]

  /**
   * @return number of nodes in the graph
   */
  def nodeCount:Int

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
  def innerNodes:GenSet[InnerNodeType]

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
  * A graph with directed zero or one edges from any single node to any other single node.
  *
  * @author dwalend
  * @since v0.1.0
  */
trait Digraph[Node] extends Graph[Node] {

  trait DigraphInnerNodeTrait extends InnerNodeTrait {
//todo remove    def value:Node

    def successors:Set[InnerEdgeType]

    def predecessors:Set[InnerEdgeType]
  }

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: DigraphInnerNodeTrait

}

/**
  * A graph with undirected zero or one edges between any pair of nodes.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait Undigraph[Node] extends Graph[Node] {

  trait UndigraphInnerNodeTrait extends InnerNodeTrait {

    def edges:Set[InnerEdgeType]

  }

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: UndigraphInnerNodeTrait

}