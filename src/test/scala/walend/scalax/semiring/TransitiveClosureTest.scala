package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import MLDiEdge._

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since v1
 */
class TransitiveClosureTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = new TransitiveClosureLabelGraphBuilder[String].initialLabelGraph(testGraph)

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

    labelGraph.edges.toOuter should be (expectedEdges)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = new TransitiveClosureLabelGraphBuilder[String].initialLabelGraph(testGraph)

    val expectedEdges = Set(
      (A~+>B)(true),
      (A~+>A)(true),
      (A~+>C)(true),
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

    TransitiveClosureSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,true)

    labelGraph.edges.toOuter should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = new TransitiveClosureLabelGraphBuilder[String].initialLabelGraph(testGraph)

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

    TransitiveClosureSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,TransitiveClosureSemiring.O)

    labelGraph.edges.toOuter should be (expectedEdges)

  }


  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(TransitiveClosureSemiring,new TransitiveClosureLabelGraphBuilder[String])(graph)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => edge.label match {
          case bool: Boolean => assert(bool, "The edge label for all self-edges should be true but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(TransitiveClosureSemiring,new TransitiveClosureLabelGraphBuilder[String])(graph)

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

    labelGraph.edges.toOuter should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(TransitiveClosure,new TransitiveClosureLabelGraphBuilder[String])(graph)

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

    labelGraph.edges.toOuter should be (expectedEdges)
  }
}
