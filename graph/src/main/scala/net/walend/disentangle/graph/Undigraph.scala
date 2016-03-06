package net.walend.disentangle.graph

/**
  * A graph with undirected zero or one edges between any pair of nodes.
  *
  * @author dwalend
  * @since v0.2.1
  */
trait Undigraph[Node] extends Graph[Node] {

  trait UndigraphInnerNodeTrait extends InnerNodeTrait {

    def innerEdges:Set[InnerEdgeType]

    def outerEdges:Set[OuterEdgeType]
  }

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: UndigraphInnerNodeTrait

}

/**
  * A pair of interchangable nodes, often used in Undigraph. Order of the nodes doesn't matter.
  *
  * @author dwalend
  * @since v0.2.1
  */
case class NodePair[+A](_1: A, _2: A) {

  def other[B >: A](node:B):A = {
    if(node == _1) _2
    else if (node == _2) _1
    else throw new IllegalArgumentException(s"This NodePair contains ${_1} and ${_2}, not node.")
  }

  def contains[B >: A](elem: B): Boolean =
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