package walend.scalax.semiring


/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v1
 */

import walend.scalax.heap.HeapOrdering

case class NextStep[N,CL](weight:CL,choices:Set[N]) {}

class AllPathsSemiring[N,CL](coreSemiring:Semiring[CL]) extends Semiring[Option[NextStep[N,CL]]] {

  type Label = Option[NextStep[N,CL]]

  //identity and annihilator
  val I = Some(NextStep[N,CL](coreSemiring.I,Set[N]()))
  val O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Label = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        val summ = coreSemiring.summary(fromThroughToSteps.weight,currentSteps.weight)
        if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
          Some(new NextStep[N,CL](currentSteps.weight,currentSteps.choices ++ fromThroughToSteps.choices))
        }
        else if (summ==fromThroughToSteps.weight) fromThroughToLabel
        else if (summ==currentSteps.weight) currentLabel
        else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentSteps.weight+" or proposed "+fromThroughToSteps.weight+" weigt.")
      }
      case (Some(fromThroughToNodes),O) => fromThroughToLabel
      case (O,Some(current)) => currentLabel
      case _ => O
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {
        Some(new NextStep[N,CL](coreSemiring.extend(fromThroughSteps.weight,throughToSteps.weight),fromThroughSteps.choices))
      }
      case _ => O
    }
  }
}

/**
 * Works if the graph labels are Doubles >= 0 and < Double.MAX_VALUE.
 */
/*
class AllLeastPathsGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                       (edgeT:originalGraph.EdgeT):MLDiEdge[M] = {
    val edge:E[M] = edgeT.toOuter
    require(edge.label.isInstanceOf[Double],"Edge labels must exist and be Doubles")
    val weight = edge.label.asInstanceOf[Double]
    require(weight >= 0,"Edge labels must be greater than 0")
    require(weight < Double.MaxValue,"Edge labels must be less than Double.MaxValue")

    (edge._1 ~+> edge._2)(Some(new NextStep(weight,Set[M](edge._2))))
  }
}

/**
 * A heap ordering that puts lower doubles on the top of the heap
 */
class AllPathsHeapOrdering[Key](coreOrdering:HeapOrdering[Key]) extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    x >= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    val diff = y-x
    if(diff>0) Some(1)
    else if (diff<0) Some(-1)
    else Some(0)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    require(key >= 0,"Key must be zero or greater, not "+key)
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Double = -1
}
*/
class AllPaths[N,CL,Key](core:GraphMinimizerSupport[CL,Key]) extends GraphMinimizerSupport[Option[NextStep[N,CL]],Key] {
  def semiring = new AllPathsSemiring(core.semiring)

  def heapOrdering = core.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => core.heapOrdering.AlwaysBottom
  }
}
