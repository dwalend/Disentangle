package net.walend.disentangle.graph.semiring

import net.walend.disentangle.heap.HeapOrdering

import scala.annotation.unused

/**
 * Finds most probable paths that traverse from start to end nodes with the on double-weight edge (weights between zero and one).
 *
 * @author dwalend
 * @since v0.1.0
 */

object MostProbable extends SemiringSupport[Double,Double] {

  def semiring: MostProbable.Semiring = MostProbableSemiring

  def heapOrdering: HeapOrdering[Double] = MostProbableOrdering

  def heapKeyForLabel: MostProbable.Label => Double = { (label:Label) => label}

  def convertEdgeToLabel[Node, Label](@unused start: Node, @unused end: Node, @unused label: Label): MostProbable.Label = semiring.I

  object MostProbableSemiring extends Semiring {

    def I = 1.0
    def O = 0.0

    def inDomain(label: Label): Boolean = {
      I >= label && label > O
    }

    def summary(fromThroughToLabel:Label,
                currentLabel:Label):Label = {
      if(fromThroughToLabel > currentLabel) {
        fromThroughToLabel
      }
      else currentLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      if ((fromThroughLabel == O) || (throughToLabel == O)) O
      else {
        fromThroughLabel * throughToLabel
      }
    }
  }

  /**
   * A heap ordering that puts lower numbers on the top of the heap
   */
  object MostProbableOrdering extends HeapOrdering[Double] {

    def lteq(x: Double, y: Double): Boolean = {
      x <= y
    }

    /**
     * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared
     */
    def tryCompare(x: Double, y: Double): Option[Int] = {
      Option(x.compareTo(y))
    }

    def keyDomainDescription = "between one and zero (the annihilator)"

    def checkKey(key: Double): Unit = {
      require(MostProbable.MostProbableSemiring.inDomain(key)||(key == MostProbable.MostProbableSemiring.O),s"Key must be $keyDomainDescription, not $key")
    }

    /**
     * Minimum value for the DoubleHeap
     */
    def AlwaysTop:Double = semiring.I + 0.01

    /**
     * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
     */
    def AlwaysBottom: Double = semiring.O
  }
}

