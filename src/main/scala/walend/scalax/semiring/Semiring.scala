package walend.scalax.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}

import LDiEdge._
import walend.scalax.heap.HeapOrdering

/**
 *
 *
 * @author dwalend
 * @since v1
 */
abstract class Semiring[Label<:AnyRef] {

  //identity
  def I:Label
  //annihilator
  def O:Label

  /**
   * Implement this method to create the core of a summary operator
   */
  //todo rework an optimized variant to return None for no change. Maybe.
  def summary(fromThroughToLabel:Label,currentLabel:Label):Label

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label

  /**
   * Override this method if you need to work with nodes and edges directly as part of your summary operator.
   */
  def fullSummary[N](labelGraph:MutableGraph[N,LDiEdge])
                       (from:labelGraph.NodeT,
                        through:labelGraph.NodeT,
                        to:labelGraph.NodeT,
                        fromThroughToLabel:Label):Label = {

    val currentLabel: Label = from ~>? to match {
      case None => O
      case Some(innerEdge) => innerEdge.label match {
        case label: Label => label
      }
    }
    summary(fromThroughToLabel, currentLabel)
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  def fullExtend[N](labelGraph:MutableGraph[N,LDiEdge])
                (fromThrough:Option[labelGraph.EdgeT],
                 throughTo:Option[labelGraph.EdgeT]):Label = {

    (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT), Some(throughToEdgeT)) => {
        val fromThroughLabel: Label = fromThroughEdgeT.label match {
          case label: Label => label
        }
        val throughToLabel: Label = throughToEdgeT.label match {
          case label: Label => label
        }
        val fromToLabel:Label = extend(fromThroughLabel,throughToLabel)
        fromToLabel
      }
      case _ => O
    }
  }

  /**
   * Override this method to add side effects when you replace a label.
   */
  //todo this really belongs in MutableGraph. Maybe "pimp my API" pattern. Definitely need it for a concurrent mutable graph.
  def replaceLabel[N](labelGraph:MutableGraph[N,LDiEdge])
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
  def relax[N](labelGraph:MutableGraph[N,LDiEdge])
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

trait LabelGraphBuilder[Label<:AnyRef] {

  def identityEdgeFromGraphNode[N](originalGraph:Graph[N,LDiEdge])
                                  (nodeT:originalGraph.NodeT)
                                  (semiring:Semiring[Label]):LDiEdge[N] = {
    val node:N = nodeT.value
    (node ~+> node)(semiring.I)
  }

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N]

  //todo when Graph.from starts using ClassTag or TypeTag, do the same. Graph.from in 0.7 uses a Manifest, and move that type parameter to the trait declaration.
  def initialLabelGraph[N:Manifest](originalGraph:Graph[N,LDiEdge])
                                   (semiring:Semiring[Label]):MutableGraph[N,LDiEdge] = {
    import scala.collection.Set

    val nodes:Set[N] = originalGraph.nodes.toNodeInSet

    val identityLabelEdges:Set[LDiEdge[N]] = originalGraph.nodes.seq.map(identityEdgeFromGraphNode(originalGraph)(_)(semiring))
    val interestingLabelEdges:Set[LDiEdge[N]] = originalGraph.edges.seq.map(initialEdgeFromGraphEdge(originalGraph))
    val initEdges:Set[LDiEdge[N]] = identityLabelEdges ++ interestingLabelEdges
    MutableGraph.from(nodes,initEdges)
  }
}

trait GraphMinimizerSupport[Label<:AnyRef,Key] {

  def semiring:Semiring[Label]

  def heapOrdering:HeapOrdering[Key]

  def heapKeyForLabel:Label => Key

}