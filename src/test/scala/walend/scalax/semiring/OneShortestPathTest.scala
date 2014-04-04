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
class OneShortestPathTest extends FlatSpec with Matchers {

  val oneShortestPath = new OneShortestPath[String]

  val expectedEdges = Set(
    (B~+>B)(None),
    (B~+>C)(Some(Step(1,Some(C)))),
    (B~+>D)(Some(Step(2,Some(C)))),
    (B~+>E)(Some(Step(3,Some(C)))),
    (B~+>F)(Some(Step(4,Some(C)))),
    (B~+>H)(Some(Step(4,Some(C)))),
    (F~+>F)(None),
    (C~+>C)(None),
    (C~+>D)(Some(Step(1,Some(D)))),
    (C~+>E)(Some(Step(2,Some(D)))),
    (C~+>F)(Some(Step(3,Some(D)))),
    (C~+>B)(Some(Step(3,Some(D)))),
    (C~+>H)(Some(Step(3,Some(D)))),
    (G~+>G)(None),
    (D~+>D)(None),
    (D~+>E)(Some(Step(1,Some(E)))),
    (D~+>F)(Some(Step(2,Some(E)))),
    (D~+>B)(Some(Step(2,Some(E)))),
    (D~+>H)(Some(Step(2,Some(E)))),
    (D~+>C)(Some(Step(3,Some(E)))),
    (H~+>H)(None),
    (H~+>C)(Some(Step(1,Some(C)))),
    (H~+>D)(Some(Step(2,Some(C)))),
    (H~+>E)(Some(Step(3,Some(C)))),
    (H~+>B)(Some(Step(4,Some(C)))),
    (H~+>F)(Some(Step(4,Some(C)))),
    (E~+>E)(None),
    (E~+>F)(Some(Step(1,Some(F)))),
    (E~+>B)(Some(Step(1,Some(B)))),
    (E~+>H)(Some(Step(1,Some(H)))),
    (E~+>C)(Some(Step(2,Some(B)))),
    (E~+>D)(Some(Step(3,Some(B)))),
    (A~+>A)(None),
    (A~+>B)(Some(Step(1,Some(B)))),
    (A~+>C)(Some(Step(2,Some(B)))),
    (A~+>D)(Some(Step(3,Some(B)))),
    (A~+>E)(Some(Step(4,Some(B)))),
    (A~+>H)(Some(Step(5,Some(B)))),
    (A~+>F)(Some(Step(5,Some(B)))))

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = new OneShortestPathGraphBuilder[String](oneShortestPath.semiring).initialLabelGraph(testGraph)

    val expectedEdges = Set(
      (B~+>B)(None),
      (B~+>C)(Some(Step(1,Some(C)))),
      (F~+>F)(None),
      (C~+>D)(Some(Step(1,Some(D)))),
      (C~+>C)(None),
      (G~+>G)(None),
      (D~+>E)(Some(Step(1,Some(E)))),
      (D~+>D)(None),
      (H~+>C)(Some(Step(1,Some(C)))),
      (H~+>H)(None),
      (E~+>F)(Some(Step(1,Some(F)))),
      (E~+>B)(Some(Step(1,Some(B)))),
      (E~+>E)(None),
      (E~+>H)(Some(Step(1,Some(H)))),
      (A~+>B)(Some(Step(1,Some(B)))),
      (A~+>A)(None)
    )

    labelGraph.edges.toOuter should be (expectedEdges)
  }

  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(oneShortestPath.semiring,new OneShortestPathGraphBuilder[String](oneShortestPath.semiring))(graph)

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

    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(oneShortestPath.semiring,new OneShortestPathGraphBuilder[String](oneShortestPath.semiring))(graph)

    val expectedEdges2 = Set(
      (B~+>B)(None),
      (B~+>F)(Some(Step(4,Some(C)))),
      (B~+>C)(Some(Step(1,Some(C)))),
      (B~+>D)(Some(Step(2,Some(C)))),
      (B~+>H)(Some(Step(4,Some(C)))),
      (B~+>E)(Some(Step(3,Some(C)))),
      (F~+>F)(None),
      (C~+>B)(Some(Step(3,Some(D)))),
      (C~+>F)(Some(Step(3,Some(D)))),
      (C~+>C)(None),
      (C~+>D)(Some(Step(1,Some(D)))),
      (C~+>H)(Some(Step(3,Some(D)))),
      (C~+>E)(Some(Step(2,Some(D)))),
      (G~+>G)(None),
      (D~+>B)(Some(Step(2,Some(E)))),
      (D~+>F)(Some(Step(2,Some(E)))),
      (D~+>C)(Some(Step(3,Some(E)))),
      (D~+>D)(None),
      (D~+>H)(Some(Step(2,Some(E)))),
      (D~+>E)(Some(Step(1,Some(E)))),
      (H~+>B)(Some(Step(4,Some(C)))),
      (H~+>F)(Some(Step(4,Some(C)))),
      (H~+>C)(Some(Step(1,Some(C)))),
      (H~+>D)(Some(Step(2,Some(C)))),
      (H~+>H)(None),
      (H~+>E)(Some(Step(3,Some(C)))),
      (E~+>B)(Some(Step(1,Some(B)))),
      (E~+>F)(Some(Step(1,Some(F)))),
      (E~+>C)(Some(Step(2,Some(B)))),
      (E~+>D)(Some(Step(3,Some(B)))),
      (E~+>H)(Some(Step(1,Some(H)))),
      (E~+>E)(None),
      (A~+>B)(Some(Step(1,Some(B)))),
      (A~+>F)(Some(Step(5,Some(B)))),
      (A~+>C)(Some(Step(2,Some(B)))),
      (A~+>D)(Some(Step(3,Some(B)))),
      (A~+>H)(Some(Step(5,Some(B)))),
      (A~+>E)(Some(Step(4,Some(B)))),
      (A~+>A)(None)
    )

    labelGraph.edges.toOuter should be (expectedEdges2)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(oneShortestPath,new OneShortestPathGraphBuilder[String](oneShortestPath.semiring))(graph)

    labelGraph.edges.toOuter should be (expectedEdges)
  }

}
