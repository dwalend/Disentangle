package net.walend.scalagraph.minimizer.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Labels are true if the sink can be reached from the source, false if not.
 *
 * @author dwalend
 * @since v0.0.0
 */
object TransitiveClosureSemiring extends Semiring[Boolean] {


  //identity and annihilator
  def I: Boolean = true
  def O: Boolean = false

  def summary(fromThroughToLabel:Boolean,
              currentLabel:Boolean):Boolean = {
    fromThroughToLabel | currentLabel
  }

  def extend(fromThroughLabel:Boolean,throughToLabel:Boolean):Boolean = {
    fromThroughLabel & throughToLabel
  }

}

import scala.reflect.runtime.universe.TypeTag
class TransitiveClosureLabelGraphBuilder[N:TypeTag] extends AbsractLabelGraphBuilder[N,Boolean](TransitiveClosureSemiring) {
  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])(edgeT: originalGraph.type#EdgeT): Boolean = TransitiveClosureSemiring.I
}

final case class TransitiveClosureHeapKey(label:Boolean, state:Int)

object TransitiveClosureHeapKey {
  val TrueKey = TransitiveClosureHeapKey(label = true,1)
  val FalseKey = TransitiveClosureHeapKey(label = false,0)
  val TopKey = TransitiveClosureHeapKey(label = true,2)

  def keyForLabel(label:Boolean):TransitiveClosureHeapKey = {
    if(label) TrueKey
    else FalseKey
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

object TransitiveClosure extends GraphMinimizerSupport[Boolean,TransitiveClosureHeapKey] {
  def semiring = TransitiveClosureSemiring

  def heapKeyForLabel = TransitiveClosureHeapKey.keyForLabel

  def heapOrdering = TransitiveClosureHeapOrdering
}
