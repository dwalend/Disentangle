package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.Graph
import MLDiEdge._

/**
 *
 *
 * @author dwalend
 * @since v1
 */
class AllShortestPathsTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (B~+>B)(Some(NextStep(0,Set()))),
    (B~+>F)(Some(NextStep(4,Set(C)))),
    (B~+>C)(Some(NextStep(1,Set(C)))),
    (B~+>D)(Some(NextStep(2,Set(C)))),
    (B~+>H)(Some(NextStep(4,Set(C)))),
    (B~+>E)(Some(NextStep(3,Set(C)))),
    (F~+>F)(Some(NextStep(0,Set()))),
    (C~+>B)(Some(NextStep(3,Set(D)))),
    (C~+>F)(Some(NextStep(3,Set(D)))),
    (C~+>C)(Some(NextStep(0,Set()))),
    (C~+>D)(Some(NextStep(1,Set(D)))),
    (C~+>H)(Some(NextStep(3,Set(D)))),
    (C~+>E)(Some(NextStep(2,Set(D)))),
    (G~+>G)(Some(NextStep(0,Set()))),
    (D~+>B)(Some(NextStep(2,Set(E)))),
    (D~+>F)(Some(NextStep(2,Set(E)))),
    (D~+>C)(Some(NextStep(3,Set(E)))),
    (D~+>D)(Some(NextStep(0,Set()))),
    (D~+>H)(Some(NextStep(2,Set(E)))),
    (D~+>E)(Some(NextStep(1,Set(E)))),
    (H~+>B)(Some(NextStep(4,Set(C)))),
    (H~+>F)(Some(NextStep(4,Set(C)))),
    (H~+>C)(Some(NextStep(1,Set(C)))),
    (H~+>D)(Some(NextStep(2,Set(C)))),
    (H~+>H)(Some(NextStep(0,Set()))),
    (H~+>E)(Some(NextStep(3,Set(C)))),
    (E~+>B)(Some(NextStep(1,Set(B)))),
    (E~+>F)(Some(NextStep(1,Set(F)))),
    (E~+>C)(Some(NextStep(2,Set(B, H)))),
    (E~+>D)(Some(NextStep(3,Set(B, H)))),
    (E~+>H)(Some(NextStep(1,Set(H)))),
    (E~+>E)(Some(NextStep(0,Set()))),
    (A~+>B)(Some(NextStep(1,Set(B)))),
    (A~+>F)(Some(NextStep(5,Set(B)))),
    (A~+>C)(Some(NextStep(2,Set(B)))),
    (A~+>D)(Some(NextStep(3,Set(B)))),
    (A~+>H)(Some(NextStep(5,Set(B)))),
    (A~+>E)(Some(NextStep(4,Set(B)))),
    (A~+>A)(Some(NextStep(0,Set()))))

  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsGraphBuilder[String])

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==allShortestPaths.semiring.I,"The edge label for all self-edges should be "+allShortestPaths.semiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsGraphBuilder[String])

    (labelGraph.edges.toOuter.to[Set] -- expectedEdges) should be (Set.empty)

    labelGraph.edges.toOuter should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPaths,new AllShortestPathsGraphBuilder[String])

    labelGraph.edges.toOuter should be (expectedEdges)
  }

}
