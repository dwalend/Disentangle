package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes. Note that Dijkstra's algorithm won't give good answers via this semiring because it finds zero or one shortest paths.
 *
 * @author dwalend
 * @since v1
 */

case class PreviousStep[N](steps:Int,predecessors:Set[N],numShortestPaths:Int) extends BrandesLabel[N] {}

class AllShortestPathsPredecessorsSemiring[N] extends Semiring[Option[PreviousStep[N]]] {

  //length of the path is length of the list

  //identity and annihilator
  def I = Some(PreviousStep[N](0,Set[N](),0))
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[PreviousStep[N]],
              currentLabel:Option[PreviousStep[N]]):Option[PreviousStep[N]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        if(fromThroughToSteps.steps < currentSteps.steps) { fromThroughToLabel }
        else if (fromThroughToSteps.steps == currentSteps.steps) {
          Some(new PreviousStep[N](currentSteps.steps,currentSteps.predecessors ++ fromThroughToSteps.predecessors,currentSteps.numShortestPaths+fromThroughToSteps.numShortestPaths))
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
  def extend(fromThroughLabel:Option[PreviousStep[N]],throughToLabel:Option[PreviousStep[N]]):Option[PreviousStep[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {
        Some(new PreviousStep[N](fromThroughSteps.steps+throughToSteps.steps,throughToSteps.predecessors,fromThroughSteps.numShortestPaths*throughToSteps.numShortestPaths))
      }
      case _ => None
    }
  }
}

class AllShortestPathsPredecessorsGraphBuilder[N] extends LabelGraphBuilder[Option[PreviousStep[N]]] {

  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  def initialEdgeFromGraphEdge[M](originalGraph:Graph[M,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[M] = {
    val edge:LDiEdge[M] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+#> edge._2)(Some(new PreviousStep(1,Set[M](edge._1),1)))
  }
}

//todo is there a good way to say in scala that this should not be used with Dijkstra's algorithm
class AllShortestPathsPredecessors[N] extends GraphMinimizerSupport[Option[PreviousStep[N]],Int] {
  def semiring = new AllShortestPathsPredecessorsSemiring[N]

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {
    case Some(previousStep) => previousStep.steps
    case None => Int.MaxValue
  }
}
