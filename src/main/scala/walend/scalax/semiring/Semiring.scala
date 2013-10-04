package walend.scalax.semiring

import scala.reflect.ClassTag
import scalax.collection.edge.LBase.LEdgeImplicits
import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}

import scalax.collection.edge.Implicits._

/**
 *
 *
 * @author dwalend
 * @since v1
 */
abstract class Semiring[Label: ClassTag] {

  object ImplicitLabel extends LEdgeImplicits[Label]
  import ImplicitLabel._

  //identity
  def I:Label
  //annihilator
  def O:Label

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Option[Label]

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
                  fromThroughToLabel:Label):Option[LDiEdge[N]] = {

    if(fromThroughToLabel != O) {
      val currentLabel:Label = from ~>? to match {
        case None => O
        case Some(edgeIn) => edgeIn.label
      }
      summary(fromThroughToLabel,currentLabel) match {
        case Some(labelUpdate) => Some((from.value ~+> to.value)(labelUpdate))
        case None => None
      }
    }
    else None
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  def fullExtend[N](labelGraph:MutableGraph[N,LDiEdge])
                (fromThrough:Option[labelGraph.EdgeT],
                 throughTo:Option[labelGraph.EdgeT]):Label = {

    (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT),Some(throughToEdgeT)) => {
        val fromThroughLabel:Label = fromThrough.get.label
        val throughToLabel:Label = throughTo.get.label

        val fromToLabel:Label = extend(fromThroughLabel,throughToLabel)
        fromToLabel
      }
      case _ => O
    }
  }

  /**
   * Override this method to add side effects when you replace an edge.
   */
  def replaceEdge[N](labelGraph:MutableGraph[N,LDiEdge])
                 (from:labelGraph.NodeT,
                  to:labelGraph.NodeT,
                  replacementEdge:LDiEdge[N]):Unit = {

    from ~>? to match {
      case Some(oldEdge) => {
        labelGraph.remove(oldEdge.toEdgeIn)
      }
      case None => ;
    }
    labelGraph.addAndGet(replacementEdge)
  }

  /**
   * Override this method to add side effects to the relax operator
   */
  def relax[N](labelGraph:MutableGraph[N,LDiEdge])
           (from:labelGraph.NodeT,
            through:labelGraph.NodeT,
            to:labelGraph.NodeT):Unit = {

    val fromThrough:Option[labelGraph.EdgeT] = from ~>? through
    val throughTo:Option[labelGraph.EdgeT] = through ~>? to
    val fromThroughToLabel:Label = fullExtend(labelGraph)(fromThrough,throughTo)

    val fromToReplace:Option[LDiEdge[N]] = fullSummary(labelGraph)(from,through,to,fromThroughToLabel)
    fromToReplace match {
      //start here. replace the edge inside the summary operator
      case Some(replacementEdge) => {
        replaceEdge(labelGraph)(from,to,replacementEdge)
      }
      case None => ;
    }
  }
}

trait LabelGraphBuilder[Label] {

  def identityEdgeFromGraphNode[N](originalGraph:Graph[N,LDiEdge])
                                  (nodeT:originalGraph.NodeT)
                                  (semiring:Semiring[Label]):LDiEdge[N] = {
    val node:N = nodeT.value
    (node ~+> node)(semiring.I)
  }

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N]

  //todo when from starts using ClassTag or TypeTag, do the same. Graph.from uses a Manifest.
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