package net.walend.disentangle.graph.semiring

import net.walend.disentangle.heap.HeapOrdering

import scala.annotation.unused

/**
 * Labels are true if the sink can be reached from the source, false if not.
 *
 * @author dwalend
 * @since v0.1.0
 */
object TransitiveClosure extends SemiringSupport[Boolean,TransitiveClosureHeapKey] {

  def semiring: TransitiveClosure.Semiring = TransitiveClosureSemiring

  def heapOrdering: HeapOrdering[TransitiveClosureHeapKey] = TransitiveClosureHeapOrdering

  def heapKeyForLabel: TransitiveClosure.Label => TransitiveClosureHeapKey = { (label:Label) => TransitiveClosureHeapKey.keyForLabel(label)}

  def convertEdgeToLabel[Node, EdgeLabel](@unused start: Node, @unused end: Node, @unused label: EdgeLabel): TransitiveClosure.Label = true

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
      Option(x.state - y.state)
    }

    def checkKey(key: TransitiveClosureHeapKey): Unit = {
      //sealed class. Nothing to check.
    }

    /**
     * Top value for the Heap if it is ever present
     */
    def AlwaysTop:TransitiveClosureHeapKey = TransitiveClosureHeapKey.TopKey

    /**
     * A key that will be among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
     */
    def AlwaysBottom: TransitiveClosureHeapKey = TransitiveClosureHeapKey.FalseKey
  }
}

sealed case class TransitiveClosureHeapKey(label:Boolean, state:Int)

object TransitiveClosureHeapKey {
  val TrueKey: TransitiveClosureHeapKey = TransitiveClosureHeapKey(label = true,1)
  val FalseKey: TransitiveClosureHeapKey = TransitiveClosureHeapKey(label = false,0)
  val TopKey: TransitiveClosureHeapKey = TransitiveClosureHeapKey(label = true,2)

  def keyForLabel(label:Boolean):TransitiveClosureHeapKey = {
    if(label) TrueKey
    else FalseKey
  }
}
