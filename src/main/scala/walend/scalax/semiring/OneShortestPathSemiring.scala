package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds a path that traverses the fewest nodes. An edge is a list of the nodes to traverse in the original graph.
 *
 * @author dwalend
 * @since v1
 */
//todo would it make more sense to have Int,Option[N] as the label?
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

class OneShortestPathGraphBuilder[N] extends LabelGraphBuilder[Option[List[N]]] {

  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge


  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+#> edge._2)(Some(List(edge._2)))
  }
}

/**
 * A heap ordering that puts lower numbers on the top of the heap
 */
object OneShortestPathGraphHeapOrdering extends HeapOrdering[Int] {

  def lteq(x: Int, y: Int): Boolean = {
    x >= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Int, y: Int): Option[Int] = {
    Some(y-x)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Int): Unit = {
    require(key >= 0,"Key must be zero or greater, not "+key)
  }

  /**
   * Minimum value for an Int heap
   */
  def AlwaysTop:Int = -1
}

class OneShortestPath[N] extends GraphMinimizerSupport[Option[List[N]],Int] {
  def semiring = new OneShortestPathSemiring[N]

  def heapOrdering = OneShortestPathGraphHeapOrdering

  def heapKeyForLabel = {label:Option[List[N]] => label match {
    case Some(list) => list.length
    case None => Int.MaxValue
  }}

}