package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */

import AllShortestPathsSemiring.NextStep

class AllShortestPathsSemiring[N] extends Semiring[Option[NextStep[N]]] {

  //identity and annihilator
  def I = Some(NextStep[N](0,Set[N]()))
  def O = None

  /**
 * Implement this method to create the core of a summary operator
 */
  def summary(fromThroughToLabel:Option[NextStep[N]],
              currentLabel:Option[NextStep[N]]):Option[NextStep[N]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        if(fromThroughToSteps.steps < currentSteps.steps) { fromThroughToLabel }
        else if (fromThroughToSteps.steps == currentSteps.steps) {
          Some(new NextStep[N](currentSteps.steps,currentSteps.choices ++ fromThroughToSteps.choices))
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
        Some(new NextStep[N](fromThroughSteps.steps+throughToSteps.steps,fromThroughSteps.choices))
      }
      case _ => None
    }
  }
}

object AllShortestPathsSemiring{
  case class NextStep[N](steps:Int,choices:Set[N]) {}
}

class AllShortestPathsGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                     (edgeT:originalGraph.EdgeT):Option[MLDiEdge[M,NextStep[M]]] = {
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
