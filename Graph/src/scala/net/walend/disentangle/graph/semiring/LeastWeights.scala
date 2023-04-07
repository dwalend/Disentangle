package net.walend.disentangle.graph.semiring

import net.walend.disentangle.heap.HeapOrdering

import scala.annotation.unused

/**
 * Finds paths that traverse from start to end nodes with the least Double-valued weight.
 *
 * @author dwalend
 * @since v0.1.0
 */

object LeastWeights extends SemiringSupport[Double,Double] {

  def semiring: LeastWeights.Semiring = LeastWeightsSemiring

  def heapOrdering: HeapOrdering[Double] = LeastWeightsOrdering

  def heapKeyForLabel: LeastWeights.Label => Double = { (label:Label) => label}

  def convertEdgeToLabel[Node, Label](@unused start: Node, @unused end: Node, @unused edge: Label): LeastWeights.Label = 1

  object LeastWeightsSemiring extends Semiring {

    def I = 0
    def O: LeastWeights.Label = Double.PositiveInfinity

    def inDomain(label: Label): Boolean = {
      I <= label && label < O
    }

    def summary(fromThroughToLabel:Label,
                currentLabel:Label):Label = {
      if(fromThroughToLabel < currentLabel) {
        fromThroughToLabel
      }
      else currentLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      if ((fromThroughLabel == O) || (throughToLabel == O)) O
      else {
        fromThroughLabel + throughToLabel
      }
    }
  }

  /**
   * A heap ordering that puts lower numbers on the top of the heap
   */
  object LeastWeightsOrdering extends HeapOrdering[Double] {

    def lteq(x: Double, y: Double): Boolean = {
      x >= y
    }

    /**
     * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared
     */
    def tryCompare(x: Double, y: Double): Option[Int] = Option(y.compareTo(x))

    def keyDomainDescription = "between zero and Double.PositiveInfinity (the annihilator)"

    def checkKey(key: Double): Unit = {
      require(LeastWeights.LeastWeightsSemiring.inDomain(key)||(key == LeastWeights.LeastWeightsSemiring.O),s"Key must be $keyDomainDescription, not $key")
    }

    /**
     * Minimum value for the DoubleHeap
     */
    def AlwaysTop:Double =  -Double.MinPositiveValue

    /**
     * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
     */
    def AlwaysBottom: Double = Double.PositiveInfinity
  }
}

