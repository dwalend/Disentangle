package net.walend.disentangle.graph

/**
  * A graph with directed zero or one edges from any single node to any other single node.
  *
  * @author dwalend
  * @since v0.1.0
  */
trait Digraph[Node] extends Graph[Node] {

  trait DigraphInnerNodeTrait extends InnerNodeTrait {

    def successors:Set[InnerEdgeType]

    def predecessors:Set[InnerEdgeType]
  }

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: DigraphInnerNodeTrait

}

/**
  * A directed graph with edges expressed as Tuple2s so that you can create edges with "a->b" in your code.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait Tuple2Digraph[Node] extends Digraph[Node] {

  type OuterEdgeType = (Node,Node)

  type InnerEdgeType = (InnerNodeType,InnerNodeType)
}