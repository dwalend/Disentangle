package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since 10/24/13 11:04 PM
 */
class TransitiveClosureTest extends FlatSpec with Matchers {

  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.graph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

    import TransitiveClosureSemiring.ImplicitLabel._

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label,"The edge label for all self-edges should be true but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }
}
