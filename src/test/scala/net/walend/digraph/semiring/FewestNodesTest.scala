package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._

/**
 * Tests algorithms with FewestNodes
 *
 * @author dwalend
 * @since v0.1.0
 */

class FewestNodesTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = ConvertToLabelDigraph.convert(testGraph,FewestNodes,FewestNodes.convertEdgeToLabel)

    val expectedEdges = Set(
      (A,B,1),
      (A,A,0),
      (B,C,1),
      (B,B,0),
      (C,C,0),
      (C,D,1),
      (D,D,0),
      (D,E,1),
      (E,B,1),
      (E,F,1),
      (E,H,1),
      (E,E,0),
      (F,F,0),
      (G,G,0),
      (H,C,1),
      (H,H,0)
    )

    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  val expectedEdges = Set(
    (E,E,0),
    (G,G,0),
    (A,F,5),
    (B,C,1),
    (D,B,2),
    (B,D,2),
    (B,B,0),
    (D,F,2),
    (H,B,4),
    (B,H,4),
    (E,D,3),
    (C,F,3),
    (A,D,3),
    (C,B,3),
    (C,C,0),
    (A,E,4),
    (H,E,3),
    (E,F,1),
    (H,C,1),
    (E,B,1),
    (C,D,1),
    (H,H,0),
    (A,B,1),
    (F,F,0),
    (A,A,0),
    (H,F,4),
    (A,H,5),
    (E,H,1),
    (E,C,2),
    (C,E,2),
    (D,C,3),
    (B,E,3),
    (A,C,2),
    (B,F,4),
    (C,H,3),
    (D,D,0),
    (D,E,1),
    (H,D,2),
    (D,H,2)
  )

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val initialGraph = ConvertToLabelDigraph.convert(testGraph,FewestNodes,FewestNodes.convertEdgeToLabel)
    val labelGraph = FloydWarshall.allPairsShortestPaths(initialGraph,FewestNodes)

    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val initialGraph = ConvertToLabelDigraph.convert(testGraph,FewestNodes,FewestNodes.convertEdgeToLabel)
    val labels = Dijkstra.allPairsShortestPaths(initialGraph,FewestNodes)

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
  }
}
