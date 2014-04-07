package walend.scalax.semiring

import scalax.collection.GraphPredef._, scalax.collection.GraphEdge.{DiEdge, EdgeCopy, NodeProduct}
import walend.scalax.semiring
//import scala.collection.GenIterable
//import scala.collection.generic.CanBuildFrom

/** Directed edge with mutable label of a given type Any. */
class MLDiEdge[N](nodes: Product)(private var _label: Any)
    extends DiEdge[N](nodes)
    with    EdgeCopy[MLDiEdge]
    with    OuterEdge[N,MLDiEdge] {

  override def label: Any = _label
  def label_=(newLabel: Any) = _label = newLabel
  override def copy[NN](newNodes: Product) = new semiring.MLDiEdge[NN](newNodes)(_label)

  override def toString():String = {
    "("+from+"~+>"+to+")" + "(" +label + ")"
  }
}

object MLDiEdge {
  
  def apply[N](from: N, to: N, label: Any) =
    new semiring.MLDiEdge[N](NodeProduct(from, to))(label)
    
  def unapply[N](e: MLDiEdge[N]) = Some(e)
  
  final implicit class Assoc[N](val n1: N) extends AnyVal {
    def ~+>(n2: N)(label: Any) = new semiring.MLDiEdge[N]((n1, n2))(label)
  }
}

object :~> {
  def unapply[N](e: MLDiEdge[N]): Option[(N, (N,Any))] =
    if (e eq null) None else Some(e._1, (e._2, e.label))
}
object + {
  def unapply[N](nl: (N, Any)): Option[(N, Any)] =
    if (nl eq null) None else Some(nl._1, nl._2)
}

private object TestMLDiEdge {
  import MLDiEdge._
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