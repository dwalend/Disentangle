package net.walend.disentangle.graph

import scala.collection.immutable.Set.Set2

/**
  * A directed graph with labeled edges.
  *
  * @author dwalend
  * @since v0.2.0
  */
trait LabelUndigraph[Node,Label] extends Undigraph[Node] {

  type OuterEdgeType = (Set2[Node],Label)

  type InnerEdgeType = (Set2[InnerNodeType],Label)

  /**
    * @return the label to return when no edge exists
    */
  def noEdgeExistsLabel:Label

  /**
    * @return the Label between a pair of nodes, or noEdgeExistsLabel if no edge exists.
    */
  def label(between:Set2[InnerNodeType]):Label
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