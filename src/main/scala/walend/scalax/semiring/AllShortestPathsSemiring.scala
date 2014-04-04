package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */

class AllShortestPaths[N] extends GraphMinimizerSupport[Option[NextStep[N,Int]],Int] {
  def semiring = new AllPathsSemiring[N,Int](CountFewestNodesSemiring)

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Option[NextStep[N,Int]] => label match {
    case Some(nextStep) => nextStep.weight
    case None => Int.MaxValue
  }}
}

import scala.reflect.runtime.universe.TypeTag
class AllShortestPathsGraphBuilder[N:TypeTag](semiring:AllPathsSemiring[N,Int]) extends LabelGraphBuilder[N,Option[NextStep[N,Int]]](semiring) {

  import scalax.collection.Graph
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])(edgeT: originalGraph.type#EdgeT): Option[NextStep[N,Int]] = {
    val edge:E[N] = edgeT.toOuter

    Some(new NextStep(1,Set[N](edge._2)))
  }
}

