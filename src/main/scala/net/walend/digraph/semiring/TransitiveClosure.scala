package net.walend.digraph.semiring

import net.walend.heap.HeapOrdering

/**
 * Labels are true if the sink can be reached from the source, false if not.
 *
 * @author dwalend
 * @since v0.1.0
 */
object TransitiveClosure extends SemiringSupport[Boolean,TransitiveClosureHeapKey] {

  def semiring = TransitiveClosureSemiring

  def heapOrdering = TransitiveClosureHeapOrdering

  def heapKeyForLabel = {label:Label => TransitiveClosureHeapKey.keyForLabel(label)}

  def convertEdgeToLabel[Node, Edge](start: Node, end: Node, edge: Edge): TransitiveClosure.Label = true

  object TransitiveClosureSemiring extends Semiring {

    def I = true
    def O = false

    def inDomain(label: Label): Boolean = true

    def summary(fromThroughToLabel:Label, currentLabel:Label):Label = {
      fromThroughToLabel || currentLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      fromThroughLabel && throughToLabel
    }
  }

  /**
   * A heap ordering that puts true above false.
   */
  object TransitiveClosureHeapOrdering extends HeapOrdering[TransitiveClosureHeapKey] {

    def lteq(x: TransitiveClosureHeapKey, y: TransitiveClosureHeapKey): Boolean = {
      x.state <= y.state
    }

    /**
     * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

     */
    def tryCompare(x: TransitiveClosureHeapKey, y: TransitiveClosureHeapKey): Option[Int] = {
      Some(x.state - y.state)
    }

    /**
     * @throws IllegalArgumentException if the key is unusable
     */
    def checkKey(key: TransitiveClosureHeapKey): Unit = {
      //sealed class. Nothing to check.
    }

    /**
     * Minimum value for the DoubleHeap
     */
    def AlwaysTop:TransitiveClosureHeapKey = TransitiveClosureHeapKey.TopKey

    /**
     * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
     */
    def AlwaysBottom: TransitiveClosureHeapKey = TransitiveClosureHeapKey.FalseKey
  }
}

sealed case class TransitiveClosureHeapKey(label:Boolean, state:Int)

object TransitiveClosureHeapKey {
  val TrueKey = TransitiveClosureHeapKey(label = true,1)
  val FalseKey = TransitiveClosureHeapKey(label = false,0)
  val TopKey = TransitiveClosureHeapKey(label = true,2)

  def keyForLabel(label:Boolean):TransitiveClosureHeapKey = {
    if(label) TrueKey
    else FalseKey
  }
}
