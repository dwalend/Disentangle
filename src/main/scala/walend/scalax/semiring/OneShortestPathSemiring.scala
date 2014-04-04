package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds a path that traverses the fewest nodes. An edge is a list of the nodes to traverse in the original graph.
 *
 * @author dwalend
 * @since v1
 */
//todo inherit from OnePath
class OneShortestPath[N] extends GraphMinimizerSupport[Option[Step[N,Int]],Int] {
  def semiring = new OnePathSemiring[N,Int](CountFewestNodesSemiring)

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Option[Step[N,Int]] => label match {
    case Some(step) => step.weight
    case None => Int.MaxValue
  }}
}

import scala.reflect.runtime.universe.TypeTag
class OneShortestPathGraphBuilder[N:TypeTag](semiring:OnePathSemiring[N,Int]) extends LabelGraphBuilder[N,Option[Step[N,Int]]](semiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])(edgeT: originalGraph.type#EdgeT): Option[Step[N,Int]] = {
    val edge:E[N] = edgeT.toOuter

    Some(new Step(1,Some(edge._2)))
  }
}
