package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

import TransitiveClosureSemiring.ImplicitLabel._

/**
 *
 *
 * @author dwalend
 * @since 11/11/13 10:45 PM
 */
class OneShortestPathTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = new OneShortestPathGraphBuilder[String].initialLabelGraph(graph)(new OneShortestPathSemiring[String])

    val expectedEdges = Set(
      (B~+#>B)(Some(List())),
      (B~+#>C)(Some(List(C))),
      (F~+#>F)(Some(List())),
      (C~+#>D)(Some(List(D))),
      (C~+#>C)(Some(List())),
      (G~+#>G)(Some(List())),
      (D~+#>E)(Some(List(E))),
      (D~+#>D)(Some(List())),
      (H~+#>C)(Some(List(C))),
      (H~+#>H)(Some(List())),
      (E~+#>F)(Some(List(F))),
      (E~+#>B)(Some(List(B))),
      (E~+#>E)(Some(List())),
      (E~+#>H)(Some(List(H))),
      (A~+#>B)(Some(List(B))),
      (A~+#>A)(Some(List()))
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.graph

    val oneShortestPath = new OneShortestPath[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(oneShortestPath.semiring)(new OneShortestPathGraphBuilder[String])

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==oneShortestPath.semiring.I,"The edge label for all self-edges should be "+oneShortestPath.semiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.graph

    val oneShortestPath = new OneShortestPath[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(oneShortestPath.semiring)(new OneShortestPathGraphBuilder[String])

    val expectedEdges = Set(
      (B~+#>B)(Some(List())),
      (B~+#>F)(Some(List(C, D, E, F))),
      (B~+#>C)(Some(List(C))),
      (B~+#>D)(Some(List(C, D))),
      (B~+#>H)(Some(List(C, D, E, H))),
      (B~+#>E)(Some(List(C, D, E))),
      (F~+#>F)(Some(List())),
      (C~+#>B)(Some(List(D, E, B))),
      (C~+#>F)(Some(List(D, E, F))),
      (C~+#>C)(Some(List())),
      (C~+#>D)(Some(List(D))),
      (C~+#>H)(Some(List(D, E, H))),
      (C~+#>E)(Some(List(D, E))),
      (G~+#>G)(Some(List())),
      (D~+#>B)(Some(List(E, B))),
      (D~+#>F)(Some(List(E, F))),
      (D~+#>C)(Some(List(E, B, C))),
      (D~+#>D)(Some(List())),
      (D~+#>H)(Some(List(E, H))),
      (D~+#>E)(Some(List(E))),
      (H~+#>B)(Some(List(C, D, E, B))),
      (H~+#>F)(Some(List(C, D, E, F))),
      (H~+#>C)(Some(List(C))),
      (H~+#>D)(Some(List(C, D))),
      (H~+#>H)(Some(List())),
      (H~+#>E)(Some(List(C, D, E))),
      (E~+#>B)(Some(List(B))),
      (E~+#>F)(Some(List(F))),
      (E~+#>C)(Some(List(B, C))),
      (E~+#>D)(Some(List(B, C, D))),
      (E~+#>H)(Some(List(H))),
      (E~+#>E)(Some(List())),
      (A~+#>B)(Some(List(B))),
      (A~+#>F)(Some(List(B, C, D, E, F))),
      (A~+#>C)(Some(List(B, C))),
      (A~+#>D)(Some(List(B, C, D))),
      (A~+#>H)(Some(List(B, C, D, E, H))),
      (A~+#>E)(Some(List(B, C, D, E))),
      (A~+#>A)(Some(List()))
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.graph

    val oneShortestPath = new OneShortestPath[String]

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(oneShortestPath,new OneShortestPathGraphBuilder[String])

    val expectedEdges = Set(
      (B~+#>B)(Some(List())),
      (B~+#>C)(Some(List(C))),
      (B~+#>D)(Some(List(C, D))),
      (B~+#>E)(Some(List(C, D, E))),
      (B~+#>F)(Some(List(C, D, E, F))),
      (B~+#>H)(Some(List(C, D, E, H))),
      (F~+#>F)(Some(List())),
      (C~+#>C)(Some(List())),
      (C~+#>D)(Some(List(D))),
      (C~+#>E)(Some(List(D, E))),
      (C~+#>F)(Some(List(D, E, F))),
      (C~+#>B)(Some(List(D, E, B))),
      (C~+#>H)(Some(List(D, E, H))),
      (G~+#>G)(Some(List())),
      (D~+#>D)(Some(List())),
      (D~+#>E)(Some(List(E))),
      (D~+#>F)(Some(List(E, F))),
      (D~+#>B)(Some(List(E, B))),
      (D~+#>H)(Some(List(E, H))),
      (D~+#>C)(Some(List(E, H, C))),
      (H~+#>H)(Some(List())),
      (H~+#>C)(Some(List(C))),
      (H~+#>D)(Some(List(C, D))),
      (H~+#>E)(Some(List(C, D, E))),
      (H~+#>B)(Some(List(C, D, E, B))),
      (H~+#>F)(Some(List(C, D, E, F))),
      (E~+#>E)(Some(List())),
      (E~+#>F)(Some(List(F))),
      (E~+#>B)(Some(List(B))),
      (E~+#>H)(Some(List(H))),
      (E~+#>C)(Some(List(B, C))),
      (E~+#>D)(Some(List(B, C, D))),
      (A~+#>A)(Some(List())),
      (A~+#>B)(Some(List(B))),
      (A~+#>C)(Some(List(B, C))),
      (A~+#>D)(Some(List(B, C, D))),
      (A~+#>E)(Some(List(B, C, D, E))),
      (A~+#>H)(Some(List(B, C, D, E, H))),
      (A~+#>F)(Some(List(B, C, D, E, F)))
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

}
