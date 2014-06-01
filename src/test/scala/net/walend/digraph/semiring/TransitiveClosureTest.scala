package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._

/**
 * Tests algorithms with FewestNodes
 *
 * @author dwalend
 * @since v0.1.0
 */

class TransitiveClosureTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (A,A,true),
    (A,B,true),
    (A,C,true),
    (A,D,true),
    (A,E,true),
    (A,F,true),
    (A,H,true),
    (B,B,true),
    (B,C,true),
    (B,D,true),
    (B,E,true),
    (B,F,true),
    (B,H,true),
    (C,B,true),
    (C,C,true),
    (C,D,true),
    (C,E,true),
    (C,F,true),
    (C,H,true),
    (D,B,true),
    (D,C,true),
    (D,D,true),
    (D,E,true),
    (D,F,true),
    (D,H,true),
    (E,B,true),
    (E,C,true),
    (E,D,true),
    (E,E,true),
    (E,F,true),
    (E,H,true),
    (F,F,true),
    (G,G,true),
    (H,B,true),
    (H,C,true),
    (H,D,true),
    (H,E,true),
    (H,F,true),
    (H,H,true)
  )

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodes,TransitiveClosure,TransitiveClosure.convertEdgeToLabel)

    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(testGraph.edges,testGraph.nodes,TransitiveClosure,TransitiveClosure.convertEdgeToLabel)

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
  }
}
