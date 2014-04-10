package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes, all previous steps to reach the destination node, and the total number of shortest paths
 *
 * @author dwalend
 * @since v1
 */
       /*
case class PreviousStep[N,CL](weight:CL,predecessors:Set[N],numShortestPaths:Int,creator:AnyRef) extends BrandesLabel[N]

/**
 *
 *
 * @param coreSemiring
 * @param willConsiderAllNodePairs should be true for the Floyd-Warshall algorithm, false for Dijkstra's and Brandes' algorithms.
 * @tparam N
 * @tparam CL
 */
class AllPathsBrandesSemiring[N,CL](coreSemiring:Semiring[CL],willConsiderAllNodePairs:Boolean = false) extends Semiring[Option[PreviousStep[N,CL]]] {

  type Label = Option[PreviousStep[N,CL]]

  //identity and annihilator
  def I = Some(PreviousStep[N,CL](coreSemiring.I,Set[N](),0,BrandesLabel.originalGraph))
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Label = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        val summ = coreSemiring.summary(fromThroughToSteps.weight,currentSteps.weight)
        if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
          Some(new PreviousStep[N,CL](currentSteps.weight,currentSteps.predecessors ++ fromThroughToSteps.predecessors,currentSteps.numShortestPaths+fromThroughToSteps.numShortestPaths,fromThroughToSteps.creator))
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
        Some(new PreviousStep[N,CL](coreSemiring.extend(fromThroughSteps.weight,throughToSteps.weight),
          throughToSteps.predecessors,
          fromThroughSteps.numShortestPaths*throughToSteps.numShortestPaths,
          BrandesLabel.default))
      }
      case _ => O
    }
  }

  def taggingExtend(fromThroughLabel:Label,
                    throughToLabel:Label,
                    creator:AnyRef):Label = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {

        if(willConsiderAllNodePairs || (fromThroughSteps.matchingCreator(creator) && throughToSteps.matchingCreator(creator))) {
          val weight = coreSemiring.extend(fromThroughSteps.weight,throughToSteps.weight)
          //if extendSteps has the same number of steps as throughToLabel, then you've moved from the origin across a first edge. Keep throughToLabel's creator
          //todo what to do with this? Something with "Does from equal the origin of the path?" maybe
//          val useCreator = if(weight <= 1) throughToSteps.creator
          val useCreator = if(weight <= 1) throughToSteps.creator
          else creator
          Some(new PreviousStep[N,CL](weight,
            throughToSteps.predecessors,
            fromThroughSteps.numShortestPaths*throughToSteps.numShortestPaths,
            useCreator))
        }
        else None
      }
      case _ => None
    }
  }

  def taggingSummary(fromThroughToLabel:Label,
                     currentLabel:Label,
                     creator:AnyRef):Label = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        //todo probably enough to have this check in taggingExtend
        if(willConsiderAllNodePairs || (fromThroughToSteps.matchingCreator(creator) && currentSteps.matchingCreator(creator))) {
          if(fromThroughToSteps.steps < currentSteps.steps) { fromThroughToLabel }
          else if (fromThroughToSteps.steps == currentSteps.steps) {

            val useCreator = if(currentSteps.steps <= 1) currentSteps.creator
            else creator
            Some(new PreviousStep[N](currentSteps.steps,currentSteps.predecessors ++ fromThroughToSteps.predecessors,currentSteps.numShortestPaths+fromThroughToSteps.numShortestPaths,useCreator))
          }
          else { currentLabel }
        }
        else {
          if(willConsiderAllNodePairs || (currentSteps.matchingCreator(creator))) currentLabel
          else None
        }
      }
      case (Some(fromThroughToNodes),None) => {
        if(willConsiderAllNodePairs || (fromThroughToNodes.matchingCreator(creator))) fromThroughToLabel
        else None
      }
      case (None,Some(current)) => {
        if(willConsiderAllNodePairs || (current.matchingCreator(creator))) currentLabel
        else None
      }
      case _ => None
    }
  }

  import scalax.collection.mutable.{Graph => MutableGraph}
  /**
   * Override this method if you need to work with nodes and edges directly as part of your summary operator.
   */
  override def fullSummary[M](labelGraph:MutableGraph[M,MLDiEdge])
                             (from:labelGraph.NodeT,
                              through:labelGraph.NodeT,
                              to:labelGraph.NodeT,
                              fromThroughToLabel:Option[PreviousStep[N,CL]]):Option[PreviousStep[N,CL]] = {

    val currentLabel:Option[PreviousStep[N,CL]] = from ~>? to match {
      case None => O
      case Some(innerEdge) => innerEdge.label.asInstanceOf[Some[PreviousStep[N,CL]]]
    }
    val result = taggingSummary(fromThroughToLabel,currentLabel,from)
    result
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  override def fullExtend[M](labelGraph:MutableGraph[M,MLDiEdge])
                            (fromThrough:Option[labelGraph.EdgeT],
                             throughTo:Option[labelGraph.EdgeT]):Option[PreviousStep[N,CL]] = {

    val result = (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT),Some(throughToEdgeT)) => {
        val fromThroughLabel:Option[PreviousStep[N,CL]] = fromThrough.get.label.asInstanceOf[Option[PreviousStep[N,CL]]]
        val throughToLabel:Option[PreviousStep[N,CL]] = throughTo.get.label.asInstanceOf[Option[PreviousStep[N,CL]]]

        val fromToLabel:Option[PreviousStep[N,CL]] = taggingExtend(fromThroughLabel,throughToLabel,fromThrough.get._1)
        fromToLabel
      }
      case _ => O
    }
    result
  }

}
/*
class AllPathsBrandes[N,CL,Key](core:GraphMinimizerSupport[CL,Key]) extends GraphMinimizerSupport[Option[NextStep[N,CL]],Key] {
  def semiring = new AllPathsBrandesSemiring(core.semiring)

  def heapOrdering = core.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => core.heapOrdering.AlwaysBottom
  }
}

//todo generalize the same way AllPathsSemiring
class AllShortestPathsPredecessorsSemiring[N](willConsiderAllNodePairs:Boolean = false) extends Semiring[Option[PreviousStep[N]]] {

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


}
/*
import scala.reflect.runtime.universe.TypeTag
class AllShortestPathsPredecessorsGraphBuilder[N:TypeTag](semiring:AllShortestPathsPredecessorsSemiring[N])
  extends LabelGraphBuilder[N,Option[PreviousStep[N]]](semiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import MLDiEdge._
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])
                                                      (edgeT: originalGraph.type#EdgeT): Option[PreviousStep[N]] = {
    val edge:E[N] = edgeT.toOuter
    Some(new PreviousStep(1,Set[N](edge._1),1,BrandesLabel.originalGraph))
  }
}

class AllShortestPathsPredecessors[N] extends GraphMinimizerSupport[Option[PreviousStep[N]],Int] {
  def semiring = new AllShortestPathsPredecessorsSemiring[N]

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {
    case Some(previousStep) => previousStep.steps
    case None => Int.MaxValue
  }
}

 */

  */*/