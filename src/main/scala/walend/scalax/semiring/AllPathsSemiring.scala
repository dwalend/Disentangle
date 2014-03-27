package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Abstract parts for finding all minimal paths
 *
 * @author dwalend
 * @since v1
 */

case class NextStep[N,W](weight:W,choices:Set[N]) {}

abstract class AllPathsSemiring[N,W] extends Semiring[Option[NextStep[N,W]]] {

  def identityWeight:W

  //identity and annihilator
  def I = Some(NextStep[N,W](identityWeight,Set[N]()))
  def O = None

  def weightOrdering:PartialOrdering[W]

  class NextStepOrdering(weightOrdering:PartialOrdering[W]) extends PartialOrdering[Option[NextStep[N,W]]] {

    override def lteq(x: Option[NextStep[N, W]], y: Option[NextStep[N, W]]): Boolean = {
      (x,y) match {
        case (Some(xx:NextStep[N,W]),Some(yy:NextStep[N,W])) => weightOrdering.lteq(xx.weight,yy.weight)
        case (Some(xx),None) => true
        case (None,Some(yy)) => false
        case (None,None) => false
      }
    }

    override def tryCompare(x: Option[NextStep[N, W]], y: Option[NextStep[N, W]]): Option[Int] = {
      (x,y) match {
        case (Some(xx:NextStep[N,W]),Some(yy:NextStep[N,W])) => weightOrdering.tryCompare(xx.weight,yy.weight)
        case (Some(xx),None) => Some(-1)
        case (None,Some(yy)) => Some(1)
        case (None,None) => Some(0)
      }
    }
  }

  lazy val ordering:NextStepOrdering = new NextStepOrdering(weightOrdering)

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[NextStep[N,W]],
              currentLabel:Option[NextStep[N,W]]):Option[NextStep[N,W]] = {

    ordering.tryCompare(fromThroughToLabel,currentLabel) match {
      case Some(x) if x == 0 => Some(new NextStep[N,W](currentLabel.get.weight,currentLabel.get.choices ++ fromThroughToLabel.get.choices))
      case Some(x) if x < 0 => fromThroughToLabel
      case Some(x) if x > 0 => currentLabel
      case None => currentLabel
    }
  }
}

class AllShortestPathsGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                       (edgeT:originalGraph.EdgeT):MLDiEdge[M] = {
    val edge:E[M] = edgeT.toOuter

    (edge._1 ~+> edge._2)(Some(new NextStep(1,Set[M](edge._2))))
  }
}

class AllShortestPaths[N] extends GraphMinimizerSupport[Option[NextStep[N]],Int] {
  def semiring = new AllShortestPathsSemiring[N]

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Option[NextStep[N]] => label match {
    case Some(nextStep) => nextStep.steps
    case None => Int.MaxValue
  }}
}
