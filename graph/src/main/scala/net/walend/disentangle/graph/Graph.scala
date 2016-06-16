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

  //todo is there a way to inject an InnerEdgeTrait that can return the outer edge??
/*
  trait InnerEdgeTrait {
    def value:OuterEdgeType
  }
*/
  type InnerEdgeType // <: InnerEdgeTrait

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
  * A graph that exposes the indices of stored nodes.
  *
  * Implementers should also include some accessor for edges via indexes.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait IndexedGraph[Node] extends Graph[Node] {

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
  trait InnerIndexedNodeTrait extends InnerNodeTrait {

    def index:Int

  }

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: InnerIndexedNodeTrait

  def node(i:Int):Node

  def innerNodeForIndex(i:Int):InnerNodeType

}