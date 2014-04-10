package walend.scalax.semiring

/**
 * Finds paths that traverse from start to end nodes with the least Double-valued weight.
 *
 * @author dwalend
 * @since v1
 */
import walend.scalax.heap.HeapOrdering

object LeastWeightsSemiring extends Semiring[Double] {

  //identity and annihilator
  def I = 0
  def O = Double.PositiveInfinity

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Double,
              currentLabel:Double):Double = {
    if(fromThroughToLabel < currentLabel) {
      fromThroughToLabel
    }
    else currentLabel
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Double,throughToLabel:Double):Double = {
    fromThroughLabel + throughToLabel
  }
}

/**
 * A heap ordering that puts lower numbers on the top of the heap
 */
object LeastWeightsHeapOrdering extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    x >= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    val diff = y-x
    if(diff>0) Some(1)
    else if (diff<0) Some(-1)
    else Some(0)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    require(key >= 0.0,"Key must be zero or greater, not "+key)
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Double = -Double.MinPositiveValue

  /**
   * A key that will be among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom: Double = Double.PositiveInfinity
}

object LeastWeights extends GraphMinimizerSupport[Double,Double] {
  def semiring = LeastWeightsSemiring

  def heapOrdering = LeastWeightsHeapOrdering

  def heapKeyForLabel = {label:Double => label}

}

/**
 * Works if the graph labels are Doubles >= 0 and <= Double.MAX_VALUE.
*/
import scala.reflect.runtime.universe.TypeTag

class LeastWeightsGraphBuilder[N:TypeTag] extends AbsractLabelGraphBuilder[N,Double](LeastWeightsSemiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])
                                                      (edgeT: originalGraph.type#EdgeT): Double = {

    val edge:E[N] = edgeT.toOuter
    require(edge.label.isInstanceOf[Double],"Edge labels must exist and be Doubles")
    val weight = edge.label.asInstanceOf[Double]
    require(weight >= 0,"Edge labels must be at least 0")
    require(weight <= Double.MaxValue,"Edge labels must be at most Double.MaxValue")

    weight
  }
}
