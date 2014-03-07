package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import java.lang.{Boolean => JBoolean}
import SomeGraph._

import scalax.collection.Graph
import LDiEdge._

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since v1
 */
class TransitiveClosureTest extends FlatSpec with Matchers {

  final def jTrue: JBoolean = true

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = TransitiveClosureLabelGraphBuilder.initialLabelGraph(testGraph)(TransitiveClosureSemiring)

    val expectedEdges = Set(
      (A~+>B)(jTrue),
      (A~+>A)(jTrue),
      (B~+>C)(jTrue),
      (B~+>B)(jTrue),
      (C~+>C)(jTrue),
      (C~+>D)(jTrue),
      (D~+>D)(jTrue),
      (D~+>E)(jTrue),
      (E~+>B)(jTrue),
      (E~+>F)(jTrue),
      (E~+>H)(jTrue),
      (E~+>E)(jTrue),
      (F~+>F)(jTrue),
      (G~+>G)(jTrue),
      (H~+>C)(jTrue),
      (H~+>H)(jTrue)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = TransitiveClosureLabelGraphBuilder.initialLabelGraph(testGraph)(TransitiveClosureSemiring)

    val expectedEdges = Set(
      (A~+>B)(jTrue),
      (A~+>A)(jTrue),
      (A~+>C)(jTrue),
      (B~+>C)(jTrue),
      (B~+>B)(jTrue),
      (C~+>C)(jTrue),
      (C~+>D)(jTrue),
      (D~+>D)(jTrue),
      (D~+>E)(jTrue),
      (E~+>B)(jTrue),
      (E~+>F)(jTrue),
      (E~+>H)(jTrue),
      (E~+>E)(jTrue),
      (F~+>F)(jTrue),
      (G~+>G)(jTrue),
      (H~+>C)(jTrue),
      (H~+>H)(jTrue)
    )

    TransitiveClosureSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,true)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = TransitiveClosureLabelGraphBuilder.initialLabelGraph(testGraph)(TransitiveClosureSemiring)

    val expectedEdges = Set(
      (A~+>B)(jTrue),
      (A~+>A)(jTrue),
      (B~+>C)(jTrue),
      (B~+>B)(jTrue),
      (C~+>C)(jTrue),
      (C~+>D)(jTrue),
      (D~+>D)(jTrue),
      (D~+>E)(jTrue),
      (E~+>B)(jTrue),
      (E~+>F)(jTrue),
      (E~+>H)(jTrue),
      (E~+>E)(jTrue),
      (F~+>F)(jTrue),
      (G~+>G)(jTrue),
      (H~+>C)(jTrue),
      (H~+>H)(jTrue)
    )

    TransitiveClosureSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,TransitiveClosureSemiring.O)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }


  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => edge.label match {
          case bool: JBoolean => assert(bool, "The edge label for all self-edges should be true but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

    val expectedEdges = Set(
      (A~+>B)(jTrue),
      (A~+>F)(jTrue),
      (A~+>C)(jTrue),
      (A~+>D)(jTrue),
      (A~+>H)(jTrue),
      (A~+>E)(jTrue),
      (A~+>A)(jTrue),
      (B~+>B)(jTrue),
      (B~+>F)(jTrue),
      (B~+>C)(jTrue),
      (B~+>D)(jTrue),
      (B~+>H)(jTrue),
      (B~+>E)(jTrue),
      (C~+>B)(jTrue),
      (C~+>F)(jTrue),
      (C~+>C)(jTrue),
      (C~+>D)(jTrue),
      (C~+>H)(jTrue),
      (C~+>E)(jTrue),
      (D~+>B)(jTrue),
      (D~+>F)(jTrue),
      (D~+>C)(jTrue),
      (D~+>D)(jTrue),
      (D~+>H)(jTrue),
      (D~+>E)(jTrue),
      (E~+>B)(jTrue),
      (E~+>F)(jTrue),
      (E~+>C)(jTrue),
      (E~+>D)(jTrue),
      (E~+>H)(jTrue),
      (E~+>E)(jTrue),
      (F~+>F)(jTrue),
      (G~+>G)(jTrue),
      (H~+>B)(jTrue),
      (H~+>F)(jTrue),
      (H~+>C)(jTrue),
      (H~+>D)(jTrue),
      (H~+>H)(jTrue),
      (H~+>E)(jTrue)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(TransitiveClosure,TransitiveClosureLabelGraphBuilder)

    val expectedEdges = Set(
      (A~+>B)(jTrue),
      (A~+>F)(jTrue),
      (A~+>C)(jTrue),
      (A~+>D)(jTrue),
      (A~+>H)(jTrue),
      (A~+>E)(jTrue),
      (A~+>A)(jTrue),
      (B~+>B)(jTrue),
      (B~+>F)(jTrue),
      (B~+>C)(jTrue),
      (B~+>D)(jTrue),
      (B~+>H)(jTrue),
      (B~+>E)(jTrue),
      (C~+>B)(jTrue),
      (C~+>F)(jTrue),
      (C~+>C)(jTrue),
      (C~+>D)(jTrue),
      (C~+>H)(jTrue),
      (C~+>E)(jTrue),
      (D~+>B)(jTrue),
      (D~+>F)(jTrue),
      (D~+>C)(jTrue),
      (D~+>D)(jTrue),
      (D~+>H)(jTrue),
      (D~+>E)(jTrue),
      (E~+>B)(jTrue),
      (E~+>F)(jTrue),
      (E~+>C)(jTrue),
      (E~+>D)(jTrue),
      (E~+>H)(jTrue),
      (E~+>E)(jTrue),
      (F~+>F)(jTrue),
      (G~+>G)(jTrue),
      (H~+>B)(jTrue),
      (H~+>F)(jTrue),
      (H~+>C)(jTrue),
      (H~+>D)(jTrue),
      (H~+>H)(jTrue),
      (H~+>E)(jTrue)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }
}
