package walend.scalax.gengraph

import scalax.collection.GraphPredef._, scalax.collection.GraphEdge.{DiEdge, EdgeCopy, NodeProduct}

/** Labeled directed edge with mutable label of a given type AnyRef. */
class LDiEdge[N](nodes: Product)(private var _label: AnyRef)
    extends DiEdge[N](nodes)
    with    EdgeCopy[LDiEdge]
    with    EdgeIn[N,LDiEdge] {

  override def label: AnyRef = _label
  def label_=(newLabel: AnyRef) = _label = newLabel
  override def copy[NN](newNodes: Product) = new LDiEdge[NN](newNodes)(_label)
}

object LDiEdge {
  
  def apply[N](from: N, to: N, label: AnyRef) =
    new LDiEdge[N](NodeProduct(from, to))(label)
    
  def unapply[N](e: LDiEdge[N]) = Some(e)
  
  final implicit class Assoc[N](val n1: N) extends AnyVal {
    def ~+>(n2: N)(label: AnyRef) = new LDiEdge[N]((n1, n2))(label)
  }
}

object :~> {
  def unapply[N](e: LDiEdge[N]): Option[(N, (N,AnyRef))] =
    if (e eq null) None else Some(e._1, (e._2, e.label))
}
object + {
  def unapply[N](nl: (N, AnyRef)): Option[(N, AnyRef)] =
    if (nl eq null) None else Some(nl._1, nl._2)
}

private object TestCustomLDiEdge {
  import LDiEdge._
  val outer = (1~+>2)(None)
  outer.label = None

  outer match {
    case n1 :~> n2 + label => 
  }
  
  import scalax.collection.Graph
  val g = Graph(outer)
  val inner = g.edges.head
  inner.label = Some(List(1,2))

  inner.edge match {
    case n1 :~> n2 + label => 
  }
}