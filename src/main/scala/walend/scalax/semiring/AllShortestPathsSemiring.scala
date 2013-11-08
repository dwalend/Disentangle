package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */

case class NextStep[N](steps:Int,choices:Set[N]) {}

class AllShortestPathsSemiring[N] extends Semiring[Option[NextStep[N]]] {

  //length of the path is length of the list

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

class AllShortestPathsGraphBuilder[N] extends LabelGraphBuilder[Option[NextStep[N]]] {

  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  //todo why must I define M different from N?
  def initialEdgeFromGraphEdge[M](originalGraph:Graph[M,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[M] = {
    val edge:LDiEdge[M] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+#> edge._2)(Some(new NextStep(1,Set[M](edge._2))))
  }
}