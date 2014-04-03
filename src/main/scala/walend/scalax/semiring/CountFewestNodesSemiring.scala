package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds the length of a path that traverses the fewest edges.
 *
 * @author dwalend
 * @since v1
 */
object CountFewestNodesSemiring extends Semiring[Int] {

  //identity and annihilator
  def I = 0
  def O = Int.MaxValue

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Int,
              currentLabel:Int):Int = {
    if(fromThroughToLabel < currentLabel) {
      fromThroughToLabel
    }
    else currentLabel
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Int,throughToLabel:Int):Int = {
    fromThroughLabel + throughToLabel
  }
}

object CountFewestNodesGraphBuilder extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[N,Label,E[X] <: EdgeLikeIn[X]](semiring:Semiring[Label])
                                                              (originalGraph:Graph[N,E])
                                                              (edgeT:originalGraph.EdgeT):MLDiEdge[N] = {
    val edge:E[N] = edgeT.toOuter

    (edge._1 ~+> edge._2)(1)
  }
}

/**
 * A heap ordering that puts lower numbers on the top of the heap
 */
object CountFewestNodesHeapOrdering extends HeapOrdering[Int] {

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
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Int = -1

  /**
   * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom: Int = Int.MaxValue
}

object CountFewestNodes extends GraphMinimizerSupport[Int,Int] {
  def semiring = CountFewestNodesSemiring

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Int => label}

}