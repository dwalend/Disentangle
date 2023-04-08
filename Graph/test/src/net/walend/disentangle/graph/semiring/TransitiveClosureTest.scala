package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.SomeGraph
import munit.FunSuite

/**
 * Tests algorithms with FewestNodes
 *
 * @author dwalend
 * @since v0.1.0
 */

class TransitiveClosureTest extends FunSuite {
  import SomeGraph.*

  val expectedArcs: Set[(String, String, Boolean)] = Set(
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

  test("The Floyd-Warshall algorithm should produce the correct label graph for Somegraph") {

    val labelGraph = FloydWarshall.allPairsLeastPaths(testDigraph.edges,Seq.from(testDigraph.nodes),TransitiveClosure,TransitiveClosure.convertEdgeToLabel)

    assertEquals(Set.from(labelGraph.edges), expectedArcs)
  }

  test("Dijkstra's algorithm should produce the correct label graph for Somegraph") {

    val edges = Dijkstra.allPairsLeastPaths(testDigraph.edges, TransitiveClosure, TransitiveClosure.convertEdgeToLabel, Seq.from(testDigraph.nodes))

    assertEquals(edges.size, expectedArcs.size)
    assertEquals(Set.from(edges), expectedArcs)
  }

  val expectedBetweenness:Map[String,Double] = Map(
    A -> 0.0,
    B -> 6.5,
    C -> 13.0,
    D -> 13.0,
    E -> 13.0,
    F -> 0.0,
    G -> 0.0,
    H -> 1.5
  )

  test("Brandes' algorithm should produce both the correct label graph and betweenness for Somegraph") {

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testDigraph.edges,Seq.from(testDigraph.nodes),TransitiveClosure,TransitiveClosure.convertEdgeToLabel)

    assertEquals(labelGraphAndBetweenness._2, expectedBetweenness)
  }
}
