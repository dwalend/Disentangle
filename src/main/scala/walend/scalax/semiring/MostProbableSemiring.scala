package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds most probable paths that traverse from start to end nodes with the on double-weight edges (with weights between zero and one).
 *
 * @author dwalend
 * @since v1
 */

object MostProbableSemiring extends Semiring[Double] {

  //identity and annihilator
  def I = 1.0
  def O = 0.0

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Double,
              currentLabel:Double):Double = {
    if(fromThroughToLabel > currentLabel) {
      fromThroughToLabel
    }
    else currentLabel
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Double,throughToLabel:Double):Double = {
    fromThroughLabel * throughToLabel
  }
}

/**
 * A heap ordering that puts higher numbers on the top of the heap
 */
object MostProbableHeapOrdering extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    y >= x
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    val diff = x-y
    if(diff>0) Some(1)
    else if (diff<0) Some(-1)
    else Some(0)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    require(key >= 0.0,"Key must be zero or greater, not "+key)
    require(key <= 1.0,"Key must be one or less, not "+key)
  }

  /**
   * A top value for the DoubleHeap
   */
  def AlwaysTop:Double = 1.01

  /**
   * A key that will be among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom: Double = 0.0
}

object MostProbable extends GraphMinimizerSupport[Double,Double] {
  def semiring = MostProbableSemiring

  def heapOrdering = MostProbableHeapOrdering

  def heapKeyForLabel = {label:Double => label}

}

/**
 * Works if the graph labels are Doubles >= 0 and <= Double.MAX_VALUE.
 */
import scala.reflect.runtime.universe.TypeTag

class MostProbableGraphBuilder[N:TypeTag] extends AbsractLabelGraphBuilder[N,Double](MostProbableSemiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])
                                                      (edgeT: originalGraph.type#EdgeT): Double = {

    val edge:E[N] = edgeT.toOuter
    require(edge.label.isInstanceOf[Double],"Edge labels must exist and be Doubles")
    val weight = edge.label.asInstanceOf[Double]
    require(weight >= 0.0,"Edge labels must be at least 0")
    require(weight <= 1.0,"Edge labels must be at most 1")

    weight
  }
}