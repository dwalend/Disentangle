package walend.scalax.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}

import MLDiEdge._
import walend.scalax.heap.HeapOrdering

/**
 *
 *
 * @author dwalend
 * @since v1
 */
abstract class Semiring[Label] {

  //identity
  def I:Label
  //annihilator
  def O:Label

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,currentLabel:Label):Label

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label

  /**
   * Override this method if you need to work with nodes and edges directly as part of your summary operator.
   */
  def fullSummary[N](labelGraph:MutableGraph[N,MLDiEdge])
                       (from:labelGraph.NodeT,
                        through:labelGraph.NodeT,
                        to:labelGraph.NodeT,
                        fromThroughToLabel:Label):Label = {

    val currentLabel: Label = from ~>? to match {
      case None => O
      case Some(innerEdge) => innerEdge.label.asInstanceOf[Label]
    }
    summary(fromThroughToLabel, currentLabel)
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  def fullExtend[N](labelGraph:MutableGraph[N,MLDiEdge])
                (fromThrough:Option[labelGraph.EdgeT],
                 throughTo:Option[labelGraph.EdgeT]):Label = {

    (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT), Some(throughToEdgeT)) => {
        val fromThroughLabel: Label = fromThroughEdgeT.label.asInstanceOf[Label]
        val throughToLabel: Label = throughToEdgeT.label.asInstanceOf[Label]
        val fromToLabel:Label = extend(fromThroughLabel,throughToLabel)
        fromToLabel
      }
      case _ => O
    }
  }

  /**
   * Override this method to add side effects when you replace a label.
   */
  def replaceLabel[N](labelGraph:MutableGraph[N,MLDiEdge])
                    (from:labelGraph.NodeT,
                     to:labelGraph.NodeT,
                     replacementLabel:Label):Unit = {

    if(replacementLabel != O) //Don't bother replacing with the annihilator
      (from ~>? to).fold[Unit](
          ifEmpty = labelGraph += (from.value ~+> to.value)(replacementLabel))(
          edge => edge.label = replacementLabel)
  }

  /**
   * Override this method to add side effects to the relax operator
   */
  def relax[N](labelGraph:MutableGraph[N,MLDiEdge])
           (from:labelGraph.NodeT,
            through:labelGraph.NodeT,
            to:labelGraph.NodeT):Label = {

    val fromThrough:Option[labelGraph.EdgeT] = from ~>? through
    val throughTo:Option[labelGraph.EdgeT] = through ~>? to
    val fromThroughToLabel:Label = fullExtend(labelGraph)(fromThrough,throughTo)

    val summaryLabel:Label = fullSummary(labelGraph)(from,through,to,fromThroughToLabel)

    replaceLabel(labelGraph)(from,to,summaryLabel)

    summaryLabel
  }
}
import scala.reflect.runtime.universe.TypeTag
//todo rename and create an interface with just initialLabelGraph and maybe initialLabelFromGraphEdge, and without the TypeTag
//N has a TypeTag to provide enough information for MutableGraph's from() method
abstract class LabelGraphBuilder[N :TypeTag,Label](semiring:Semiring[Label]) {
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def identityEdgeFromGraphNode[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E])
                                  (nodeT:originalGraph.NodeT):MLDiEdge[N] = {
    val node:N = nodeT.value
    (node ~+> node)(semiring.I)
  }


  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E])
                                                        (edgeT:originalGraph.EdgeT):Label

  def initialEdgeFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E])
                                                        (edgeT:originalGraph.EdgeT):MLDiEdge[N] = {
    val edge:E[N] = edgeT.toOuter

    (edge._1 ~+> edge._2)(initialLabelFromGraphEdge(originalGraph)(edgeT))
  }

  def initialLabelGraph[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E]):MutableGraph[N,MLDiEdge] = {
    import scala.collection.Set

    val nodes:Set[N] = originalGraph.nodes.toOuter

    val identityLabelEdges:Set[MLDiEdge[N]] = originalGraph.nodes.seq.map(identityEdgeFromGraphNode(originalGraph)(_))
    val interestingLabelEdges:Set[MLDiEdge[N]] = originalGraph.edges.seq.map(initialEdgeFromGraphEdge(originalGraph))
    val initEdges:Set[MLDiEdge[N]] = identityLabelEdges ++ interestingLabelEdges
    MutableGraph.from(nodes,initEdges)
  }
}

trait GraphMinimizerSupport[Label,Key] {

  def semiring:Semiring[Label]

  def heapOrdering:HeapOrdering[Key]

  def heapKeyForLabel:Label => Key

}