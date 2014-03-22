package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds a path that traverses the fewest nodes. An edge is a list of the nodes to traverse in the original graph.
 *
 * @author dwalend
 * @since v1
 */
class OneShortestPathSemiring[N] extends Semiring[Option[List[N]]] {

  //length of the path is length of the list

  //identity and annihilator
  def I = Some(List[N]())
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[List[N]],
              currentLabel:Option[List[N]]):Option[List[N]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToNodes),Some(currentNodes)) => {
        if(fromThroughToNodes.size < currentNodes.size) fromThroughToLabel
        else currentLabel
      }
      case (Some(fromThroughToNodes),None) => fromThroughToLabel
      case (None,Some(currentNodes)) => currentLabel
      case _ => O
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Option[List[N]],throughToLabel:Option[List[N]]):Option[List[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughNodes),Some(throughToNodes)) => {
        val fromToLabel:Option[List[N]] = Some(fromThroughNodes ++ throughToNodes)
        fromToLabel
      }
      case _ => None
    }
  }
}

class OneShortestPathGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                       (edgeT:originalGraph.EdgeT):MLDiEdge[M] = {
    val edge:E[M] = edgeT.toOuter

    (edge._1 ~+> edge._2)(Some(List(edge._2)))
  }
}

class OneShortestPath[N] extends GraphMinimizerSupport[Option[List[N]],Int] {
  def semiring = new OneShortestPathSemiring[N]

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Option[List[N]] => label match {
    case Some(list) => list.length
    case None => Int.MaxValue
  }}

}