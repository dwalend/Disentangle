package net.walend.disentangle.graph.semiring

import net.walend.disentangle.heap.HeapOrdering

import scala.annotation.unused

/**
 * Finds the length of a path that traverses the fewest edges.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FewestNodes extends SemiringSupport[Int,Int] {

  def semiring: FewestNodes.Semiring = FewestNodesSemiring

  def heapOrdering: HeapOrdering[Int] = FewestNodesHeapOrdering

  def heapKeyForLabel: FewestNodes.Label => Int = { (label:Label) => label}

  def convertEdgeToLabel(@unused start: Any, @unused end: Any, @unused label: Any): FewestNodes.Label = 1

  val edgeToLabelConverter:(Any,Any,Any) => Int = convertEdgeToLabel

  object FewestNodesSemiring extends Semiring {

    def I = 0
    def O: FewestNodes.Label = Int.MaxValue

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
        val result = fromThroughLabel + throughToLabel
        if(result < 0) O
        else result
      }
    }
  }

  /**
   * A heap ordering that puts lower numbers on the top of the heap
   */
  object FewestNodesHeapOrdering extends HeapOrdering[Int] {

    def lteq(x: Int, y: Int): Boolean = {
      x >= y
    }

    /**
     * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared
     */
    def tryCompare(x: Int, y: Int): Option[Int] = {
      Option(y-x)
    }

    def keyDomainDescription = "between zero and Int.MaxValue (the annihilator)"

    def checkKey(key: Int): Unit = {
      require(FewestNodes.FewestNodesSemiring.inDomain(key)||(key == FewestNodes.FewestNodesSemiring.O),s"Key must be $keyDomainDescription, not $key")
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

}