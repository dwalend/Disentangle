package walend.scalax.semiring

/**
 * Finds all most-probable paths that traverse between two nodes based on double-weight edges (with weights between zero and one).
 *
 * @author dwalend
 * @since v1
 */

import MostProbablePathsSemiring.NextStep
import walend.scalax.heap.HeapOrdering

class MostProbablePathsSemiring[N] extends Semiring[Option[NextStep[N]]] {

  //identity and annihilator
  def I = Some(NextStep[N](weight = 0.0,Set[N]()))
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[NextStep[N]],
              currentLabel:Option[NextStep[N]]):Option[NextStep[N]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        //todo care about epsilon ?
        if(fromThroughToSteps.weight > currentSteps.weight) { fromThroughToLabel }
        else if (fromThroughToSteps.weight == currentSteps.weight) {
          Some(new NextStep[N](currentSteps.weight,currentSteps.choices ++ fromThroughToSteps.choices))
        }
        else { currentLabel }
      }
      case (Some(fromThroughToNodes),None) => fromThroughToLabel
      case (None,Some(current)) => currentLabel
      case _ => None
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Option[NextStep[N]],throughToLabel:Option[NextStep[N]]):Option[NextStep[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {
        Some(new NextStep[N](fromThroughSteps.weight * throughToSteps.weight,fromThroughSteps.choices))
      }
      case _ => None
    }
  }
}

object MostProbablePathsSemiring{
  case class NextStep[N](weight:Double,choices:Set[N]) {}
}

/**
 * Works if the graph labels are Doubles >= 0 and < Double.MAX_VALUE.
 */
class MostProbablePathsGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                       (edgeT:originalGraph.EdgeT):MLDiEdge[M] = {
    val edge:E[M] = edgeT.toOuter
    require(edge.label.isInstanceOf[Double],"Edge labels must exist and be Doubles")
    val weight = edge.label.asInstanceOf[Double]
    require(weight >= 0,"Edge labels must be at least 0")
    require(weight <= 1,"Edge labels must be at most 1")

    (edge._1 ~+> edge._2)(Some(new NextStep(weight,Set[M](edge._2))))
  }
}

/**
 * A heap ordering that puts higher doubles on the top of the heap
 */
object MostProbablePathsHeapOrdering extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    x <= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared
   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    val diff = x-y
    if(diff>0) Some(1)
    else if (diff<0) Some(-1)
    else Some(0)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    require(key >= 0,"Key must be zero or greater, not "+key)
    require(key <= 1,"Key must be one or less, not "+key)
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Double = 2.0
}

class MostProbablePaths[N] extends GraphMinimizerSupport[Option[NextStep[N]],Double] {
  def semiring = new MostProbablePathsSemiring[N]

  def heapOrdering = MostProbablePathsHeapOrdering

  def heapKeyForLabel = {label:Option[NextStep[N]] => label match {
    case Some(nextStep) => nextStep.weight
    case None => Double.MaxValue
  }}
}
