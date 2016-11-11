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
  override type InnerNodeType <: DigraphInnerNodeTrait

  trait DigraphInnerEdgeTrait extends InnerEdgeTrait {
    def from:InnerNodeType
    def to:InnerNodeType

    override def selfEdge = {from == to}

    override def other(node:InnerNodeType) = {
      if(node == from) to
      else if (node == to) from
      else throw new IllegalArgumentException(s"This edge contains ${from} and ${to}, not $node.")
    }

  }

  override type InnerEdgeType <: DigraphInnerEdgeTrait

  def edge(from: InnerNodeType,to: InnerNodeType):Option[InnerEdgeType]

  def edge(from: Node, to: Node): Option[InnerEdgeType] = {
    val inFrom = innerNode(from)
    val inTo = innerNode(to)
    (inFrom, inTo) match {
      case (Some(f), Some(t)) => edge(f, t)
      case _ => None
    }
  }

}

/**
  * A directed graph with edges expressed as Tuple2s so that you can create edges with "a->b" in your code.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait Tuple2Digraph[Node] extends Digraph[Node] {
  type OuterEdgeType = (Node,Node)
}