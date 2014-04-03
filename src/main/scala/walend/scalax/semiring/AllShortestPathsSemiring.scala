package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */

class AllShortestPathsSemiring[N] extends AllPathsSemiring[N,Int](CountFewestNodesSemiring) {

}

class AllShortestPathsGraphBuilder[N](override val semiring:AllShortestPathsSemiring[N]) extends LabelGraphBuilder[Option[NextStep[N,Int]]] {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialLabelFromGraphEdge[M <: N, E[X] <: EdgeLikeIn[X]](originalGraph: Graph[M, E])(edgeT: originalGraph.type#EdgeT): Option[NextStep[N,Int]] = {
    val edge:E[M] = edgeT.toOuter

    Some(new NextStep(1,Set[N](edge._2)))
  }
}

class AllShortestPaths[N] extends GraphMinimizerSupport[Option[NextStep[N,Int]],Int] {
  def semiring = new AllShortestPathsSemiring[N]

  def heapOrdering = CountFewestNodesHeapOrdering

  def heapKeyForLabel = {label:Option[NextStep[N,Int]] => label match {
    case Some(nextStep) => nextStep.weight
    case None => Int.MaxValue
  }}
}
