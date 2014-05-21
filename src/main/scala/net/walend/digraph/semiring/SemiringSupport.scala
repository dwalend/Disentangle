package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Parts for semiring-based graph minimizing algorithms.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait SemiringSupport[L,Key] {

  type Label = L

  def semiring:Semiring

  def heapOrdering:HeapOrdering[Key]

  def heapKeyForLabel:Label => Key

  trait Semiring {

    /** identity */
    def I:Label
    /** annihilator */
    def O:Label

    /**
     * true if the value is within the Semiring's domain
     */
    def inDomain(label:Label):Boolean

    /**
     * Implement this method to create the core of a summary operator
     */
    def summary(fromThroughToLabel:Label,currentLabel:Label):Label

    /**
     * Implement this method to create the core of an extend operator
     */
    def extend(fromThroughLabel:Label,throughToLabel:Label):Label

    /**
     * Override this method to add side effects to the relax operator
     */

  }
}
