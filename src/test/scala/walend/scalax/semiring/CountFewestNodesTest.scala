package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

import TransitiveClosureSemiring.ImplicitLabel._

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since 11/7/13 10:31 PM
 */
class CountFewestNodesTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(graph)(CountFewestNodesSemiring)

    //todo it looks like edge equality doesn't include checking labels !?
    val expectedEdges = Set(
      (A~+>B)(1),
      (A~+>A)(0),
      (B~+>C)(1),
      (B~+>B)(0),
      (C~+>C)(0),
      (C~+>D)(1),
      (D~+>D)(0),
      (D~+>E)(1),
      (E~+>B)(1),
      (E~+>F)(1),
      (E~+>H)(1),
      (E~+>E)(0),
      (F~+>F)(0),
      (G~+>G)(0),
      (H~+>C)(1),
      (H~+>H)(0)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(graph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(1),
      (A~+>A)(0),
      (B~+>C)(1),
      (B~+>B)(0),
      (C~+>C)(0),
      (C~+>D)(1),
      (D~+>D)(0),
      (D~+>E)(1),
      (E~+>B)(1),
      (E~+>F)(1),
      (E~+>H)(1),
      (E~+>E)(0),
      (F~+>F)(0),
      (G~+>G)(0),
      (H~+>C)(1),
      (H~+>H)(0),
      (A~+>C)(3)
    )

    CountFewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,3)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(graph)(CountFewestNodesSemiring)

    //todo the labels don't seem to matter in the equals...
    val expectedEdges = Set(
      (A~+>B)(true),
      (A~+>A)(true),
      (B~+>C)(true),
      (B~+>B)(true),
      (C~+>C)(true),
      (C~+>D)(true),
      (D~+>D)(true),
      (D~+>E)(true),
      (E~+>B)(true),
      (E~+>F)(true),
      (E~+>H)(true),
      (E~+>E)(true),
      (F~+>F)(true),
      (G~+>G)(true),
      (H~+>C)(true),
      (H~+>H)(true)
    )

    CountFewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,CountFewestNodesSemiring.O)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }


  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.graph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesSemiring)(CountFewestNodesGraphBuilder)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==CountFewestNodesSemiring.I,"The edge label for all self-edges should be "+CountFewestNodesSemiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.graph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesSemiring)(CountFewestNodesGraphBuilder)

    val expectedEdges = Set(
      (A~+>B)(true),
      (A~+>F)(true),
      (A~+>C)(true),
      (A~+>D)(true),
      (A~+>H)(true),
      (A~+>E)(true),
      (A~+>A)(true),
      (B~+>B)(true),
      (B~+>F)(true),
      (B~+>C)(true),
      (B~+>D)(true),
      (B~+>H)(true),
      (B~+>E)(true),
      (C~+>B)(true),
      (C~+>F)(true),
      (C~+>C)(true),
      (C~+>D)(true),
      (C~+>H)(true),
      (C~+>E)(true),
      (D~+>B)(true),
      (D~+>F)(true),
      (D~+>C)(true),
      (D~+>D)(true),
      (D~+>H)(true),
      (D~+>E)(true),
      (E~+>B)(true),
      (E~+>F)(true),
      (E~+>C)(true),
      (E~+>D)(true),
      (E~+>H)(true),
      (E~+>E)(true),
      (F~+>F)(true),
      (G~+>G)(true),
      (H~+>B)(true),
      (H~+>F)(true),
      (H~+>C)(true),
      (H~+>D)(true),
      (H~+>H)(true),
      (H~+>E)(true)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.graph

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(CountFewestNodes,CountFewestNodesGraphBuilder)

    val expectedEdges = Set(
      (A~+>B)(true),
      (A~+>F)(true),
      (A~+>C)(true),
      (A~+>D)(true),
      (A~+>H)(true),
      (A~+>E)(true),
      (A~+>A)(true),
      (B~+>B)(true),
      (B~+>F)(true),
      (B~+>C)(true),
      (B~+>D)(true),
      (B~+>H)(true),
      (B~+>E)(true),
      (C~+>B)(true),
      (C~+>F)(true),
      (C~+>C)(true),
      (C~+>D)(true),
      (C~+>H)(true),
      (C~+>E)(true),
      (D~+>B)(true),
      (D~+>F)(true),
      (D~+>C)(true),
      (D~+>D)(true),
      (D~+>H)(true),
      (D~+>E)(true),
      (E~+>B)(true),
      (E~+>F)(true),
      (E~+>C)(true),
      (E~+>D)(true),
      (E~+>H)(true),
      (E~+>E)(true),
      (F~+>F)(true),
      (G~+>G)(true),
      (H~+>B)(true),
      (H~+>F)(true),
      (H~+>C)(true),
      (H~+>D)(true),
      (H~+>H)(true),
      (H~+>E)(true)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }
}
