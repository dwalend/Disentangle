package walend.scalax.semiring

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.{DiEdge, EdgeCopy, NodeProduct}

/**
 * A directed edge with a mutable label of a specified type.
 *
 * @author Peter Empen
 */

class MLDiEdge[N,@specialized L](nodes: Product)(private var _label: L)
  extends DiEdge[N](nodes)
  with    EdgeCopy [  ({type λ[α] = MLDiEdge[α,L]})#λ]
  with    OuterEdge[N,({type λ[α] = MLDiEdge[α,L]})#λ] {

  override def label: L = _label
  def label_=(newLabel: L) = _label = newLabel
  override def copy[NN](newNodes: Product) = new MLDiEdge[NN,L](newNodes)(_label)

  override protected def attributesToString = s" '$label"
}

object MLDiEdge {

  def apply[N,L](from: N, to: N, label: L) =
    new MLDiEdge[N,L](NodeProduct(from, to))(label)

  def unapply[N,L](e: MLDiEdge[N,L]) = Some(e)

  final implicit class Assoc[N](val n1: N) extends AnyVal {
    def ~+>[L](n2: N)(label: L) = new MLDiEdge[N,L]((n1, n2))(label)
  }
}

object :~> {
  def unapply[N,L](e: MLDiEdge[N,L]): Option[(N, (N,L))] =
    if (e eq null) None else Some(e._1, (e._2, e.label))
}
object + {
  def unapply[N,L](nl: (N, L)): Option[(N, L)] =
    if (nl eq null) None else Some(nl._1, nl._2)
}

object DemoMLDiEdge extends App {
  import MLDiEdge._
  val outer = (1~+>2)(None: Option[List[Int]])
  outer.label = None

  outer match {
    case n1 :~> n2 + label =>
  }

  import scalax.collection.Graph
  type E[X] = MLDiEdge[X,Option[List[Int]]]

  val g = Graph[Int,E](outer)
  val inner = g.edges.head

  print(s"Let's mutate [$inner]")
  inner.label = Some(List(1,2))
  println(s" to [$inner]")

  inner.edge match {
    case _ :~> _ + label => println(s"matched label = $label")
  }
}


/*
class MLDiEdge[N](nodes: Product)(private var _label: Any)
    extends DiEdge[N](nodes)
    with    EdgeCopy[MLDiEdge]
    with    OuterEdge[N,MLDiEdge] {

  override def label: Any = _label
  def label_=(newLabel: Any) = _label = newLabel
  override def copy[NN](newNodes: Product) = new semiring.MLDiEdge[NN](newNodes)(_label)
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
*/