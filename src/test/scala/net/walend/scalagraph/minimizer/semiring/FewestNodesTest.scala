package net.walend.scalagraph.minimizer.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import MLDiEdge._

import net.walend.scalagraph.minimizer.gengraph.GraphFactory
import scala.util.Random

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since v1
 */
class FewestNodesTest extends FlatSpec with Matchers {
  
  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = new FewestNodesGraphBuilder[String].initialLabelGraph(testGraph)

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

    labelGraph.edges.toOuter should be (expectedEdges)
    EdgeHelp.checkEdgeSets(labelGraph.edges.toOuter.asInstanceOf[Set[MLDiEdge[String]]],expectedEdges) should be (None)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = new FewestNodesGraphBuilder[String].initialLabelGraph(testGraph)

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

    FewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,3)

    labelGraph.edges.toOuter should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = new FewestNodesGraphBuilder[String].initialLabelGraph(testGraph)

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

    FewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,FewestNodesSemiring.O)

    labelGraph.edges.toOuter should be (expectedEdges)
    EdgeHelp.checkEdgeSets(labelGraph.edges.toOuter.asInstanceOf[Set[MLDiEdge[String]]],expectedEdges) should be (None)
  }


  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(FewestNodesSemiring,new FewestNodesGraphBuilder[String])(graph)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==FewestNodesSemiring.I,"The edge label for all self-edges should be "+FewestNodesSemiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  val expectedEdges = Set(
    (E~+>E)(0),
    (G~+>G)(0),
    (A~+>F)(5),
    (B~+>C)(1),
    (D~+>B)(2),
    (B~+>D)(2),
    (B~+>B)(0),
    (D~+>F)(2),
    (H~+>B)(4),
    (B~+>H)(4),
    (E~+>D)(3),
    (C~+>F)(3),
    (A~+>D)(3),
    (C~+>B)(3),
    (C~+>C)(0),
    (A~+>E)(4),
    (H~+>E)(3),
    (E~+>F)(1),
    (H~+>C)(1),
    (E~+>B)(1),
    (C~+>D)(1),
    (H~+>H)(0),
    (A~+>B)(1),
    (F~+>F)(0),
    (A~+>A)(0),
    (H~+>F)(4),
    (A~+>H)(5),
    (E~+>H)(1),
    (E~+>C)(2),
    (C~+>E)(2),
    (D~+>C)(3),
    (B~+>E)(3),
    (A~+>C)(2),
    (B~+>F)(4),
    (C~+>H)(3),
    (D~+>D)(0),
    (D~+>E)(1),
    (H~+>D)(2),
    (D~+>H)(2)
  )

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(FewestNodesSemiring,new FewestNodesGraphBuilder[String])(graph)


    labelGraph.edges.toOuter should be (expectedEdges)
    EdgeHelp.checkEdgeSets(labelGraph.edges.toOuter.asInstanceOf[Set[MLDiEdge[String]]],expectedEdges) should be (None)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(FewestNodes,new FewestNodesGraphBuilder[String])(graph)

    labelGraph.edges.toOuter should be (expectedEdges)
    EdgeHelp.checkEdgeSets(labelGraph.edges.toOuter.asInstanceOf[Set[MLDiEdge[String]]],expectedEdges) should be (None)
  }
}
