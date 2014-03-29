package walend.scalax.semiring

/**
 * Finds all paths that traverse the fewest nodes.
 *
 * @author dwalend
 * @since v1
 */

class AllShortestPathsSemiring[N] extends AllPathsSemiring[N,Int](CountFewestNodesSemiring) {

}

class AllShortestPathsGraphBuilder[N] extends LabelGraphBuilder {

  import scalax.collection.Graph
  import MLDiEdge._
  import scalax.collection.GraphPredef.EdgeLikeIn
  import scala.language.higherKinds

  def initialEdgeFromGraphEdge[M,E[X] <: EdgeLikeIn[X]](originalGraph:Graph[M,E])
                                                     (edgeT:originalGraph.EdgeT):MLDiEdge[M] = {
  val edge:E[M] = edgeT.toOuter

  (edge._1 ~+> edge._2)(Some(new NextStep(1,Set[M](edge._2))))
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
