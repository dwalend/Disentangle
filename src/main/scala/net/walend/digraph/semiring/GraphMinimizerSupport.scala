package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Parts for semiring-based graph minimizing algorithms.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait GraphMinimizerSupport[L,Key] {

  type Label = L

  def semiring:Semiring[Label]

  def heapOrdering:HeapOrdering[Key]

  def heapKeyForLabel:Label => Key

}