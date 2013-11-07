package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Labels are true if the sink can be reached from the source, false if not.
 *
 * @author dwalend
 * @since v1
 */
object TransitiveClosureSemiring extends Semiring[Boolean] {


  //identity and annihilator
  def I = true
  def O = false

  def summary(fromThroughToLabel:Boolean,
              currentLabel:Boolean):Boolean = {
    fromThroughToLabel | currentLabel
  }

  def extend(fromThroughLabel:Boolean,throughToLabel:Boolean):Boolean = {
    fromThroughLabel & throughToLabel
  }

}

object TransitiveClosureLabelGraphBuilder extends LabelGraphBuilder[Boolean] {
  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+> edge._2)(TransitiveClosureSemiring.I)
  }
}

final case class TransitiveClosureHeapKey(override val label:Boolean, state:Int) extends HeapKey[Boolean]

object TransitiveClosureHeapKey {
  val TrueKey = TransitiveClosureHeapKey(true,1)
  val FalseKey = TransitiveClosureHeapKey(false,0)
  val TopKey = TransitiveClosureHeapKey(true,2)

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
}

object TransitiveClosure extends GraphMinimizerSupport[Boolean,TransitiveClosureHeapKey] {
  def semiring = TransitiveClosureSemiring

  def heapKeyForLabel = TransitiveClosureHeapKey.keyForLabel

  def heapOrdering = TransitiveClosureHeapOrdering
}
