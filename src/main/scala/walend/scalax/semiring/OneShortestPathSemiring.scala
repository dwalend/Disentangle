package walend.scalax.semiring

import walend.scalax.heap.HeapOrdering

/**
 * Finds a path that traverses the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */
class OneShortestPath[N] extends OnePath[N,Int,Int](CountFewestNodes)

import scala.reflect.runtime.universe.TypeTag
class OneShortestPathGraphBuilder[N:TypeTag](semiring:OnePathSemiring[N,Int]) extends AbsractLabelGraphBuilder[N,Option[Step[N,Int]]](semiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])(edgeT: originalGraph.type#EdgeT): Option[Step[N,Int]] = {
    val edge:E[N] = edgeT.toOuter

    Some(new Step(1,Some(edge._2)))
  }
}
