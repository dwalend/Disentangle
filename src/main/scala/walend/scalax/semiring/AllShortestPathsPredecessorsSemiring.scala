package walend.scalax.semiring

import scalax.collection.mutable.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.LBase.LEdgeImplicits

/**
 * Finds all paths that traverse the fewest nodes. Note that Dijkstra's algorithm won't give good answers via this semiring because it finds zero or one shortest paths.
 *
 * @author dwalend
 * @since v1
 */

case class PreviousStep[N](steps:Int,predecessors:Set[N],numShortestPaths:Int,creator:AnyRef) extends BrandesLabel[N] {}

class AllShortestPathsPredecessorsSemiring[N] extends Semiring[Option[PreviousStep[N]]] {

  object AnotherImplicitLabel extends LEdgeImplicits[Option[PreviousStep[N]]]
  import AnotherImplicitLabel._

  //identity and annihilator
  def I = Some(PreviousStep[N](0,Set[N](),0,BrandesLabel.originalGraph))
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
          Some(new PreviousStep[N](currentSteps.steps,currentSteps.predecessors ++ fromThroughToSteps.predecessors,currentSteps.numShortestPaths+fromThroughToSteps.numShortestPaths,fromThroughToSteps.creator))
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
        Some(new PreviousStep[N](fromThroughSteps.steps+throughToSteps.steps,
                                  throughToSteps.predecessors,
                                  fromThroughSteps.numShortestPaths*throughToSteps.numShortestPaths,
                                  BrandesLabel.default))
      }
      case _ => None
    }
  }

  def taggingExtend(fromThroughLabel:Option[PreviousStep[N]],
                    throughToLabel:Option[PreviousStep[N]],
                    creator:AnyRef):Option[PreviousStep[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {

        if(fromThroughSteps.matchingCreator(creator) && throughToSteps.matchingCreator(creator)) {
          val numberSteps = fromThroughSteps.steps+throughToSteps.steps
          //if extendSteps has the same number of steps as throughToLabel, then you've moved from the origin across a first edge. Keep throughToLabel's creator
          val useCreator = if(numberSteps <= 1) throughToSteps.creator
                           else creator
          Some(new PreviousStep[N](numberSteps,
            throughToSteps.predecessors,
            fromThroughSteps.numShortestPaths*throughToSteps.numShortestPaths,
            useCreator))
        }
        else None
      }
      case _ => None
    }
  }

  def taggingSummary(fromThroughToLabel:Option[PreviousStep[N]],
                      currentLabel:Option[PreviousStep[N]],
                      creator:AnyRef):Option[PreviousStep[N]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        //todo probably enough to have this check in taggingExtend
        if(fromThroughToSteps.matchingCreator(creator) && currentSteps.matchingCreator(creator)) {
          if(fromThroughToSteps.steps < currentSteps.steps) { fromThroughToLabel }
          else if (fromThroughToSteps.steps == currentSteps.steps) {

            val useCreator = if(currentSteps.steps <= 1) currentSteps.creator
                              else creator
            Some(new PreviousStep[N](currentSteps.steps,currentSteps.predecessors ++ fromThroughToSteps.predecessors,currentSteps.numShortestPaths+fromThroughToSteps.numShortestPaths,useCreator))
          }
          else { currentLabel }
        }
        else {
          if(currentSteps.matchingCreator(creator)) currentLabel
          else None
        }
      }
      case (Some(fromThroughToNodes),None) => {
        if(fromThroughToNodes.matchingCreator(creator)) fromThroughToLabel
        else None
      }
      case (None,Some(current)) => {
        if(current.matchingCreator(creator)) currentLabel
        else None
      }
      case _ => None
    }
  }

  import scalax.collection.mutable.{Graph => MutableGraph}
  /**
   * Override this method if you need to work with nodes and edges directly as part of your summary operator.
   */
  override def fullSummary[M](labelGraph:MutableGraph[M,LDiEdge])
                            (from:labelGraph.NodeT,
                             through:labelGraph.NodeT,
                             to:labelGraph.NodeT,
                             fromThroughToLabel:Option[PreviousStep[N]]):Option[PreviousStep[N]] = {

    val currentLabel:Option[PreviousStep[N]] = from ~>? to match {
      case None => O
      case Some(edgeIn) => edgeIn.label
    }
    val result = taggingSummary(fromThroughToLabel,currentLabel,from)
    result
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  override def fullExtend[M](labelGraph:MutableGraph[M,LDiEdge])
                 (fromThrough:Option[labelGraph.EdgeT],
                  throughTo:Option[labelGraph.EdgeT]):Option[PreviousStep[N]] = {

    val result = (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT),Some(throughToEdgeT)) => {
        //todo don't cast
        val fromThroughLabel:Option[PreviousStep[N]] = fromThrough.get.label.asInstanceOf[Option[PreviousStep[N]]]
        val throughToLabel:Option[PreviousStep[N]] = throughTo.get.label.asInstanceOf[Option[PreviousStep[N]]]

        val fromToLabel:Option[PreviousStep[N]] = taggingExtend(fromThroughLabel,throughToLabel,fromThrough.get._1)
        fromToLabel
      }
      case _ => O
    }
    result
  }


}

class AllShortestPathsPredecessorsGraphBuilder[N] extends LabelGraphBuilder[Option[PreviousStep[N]]] {

  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  def initialEdgeFromGraphEdge[M](originalGraph:Graph[M,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[M] = {
    val edge:LDiEdge[M] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+#> edge._2)(Some(new PreviousStep(1,Set[M](edge._1),1,BrandesLabel.originalGraph)))
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
