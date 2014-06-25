package net.walend.graph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.graph.SomeGraph._

/**
 * Tests algorithms with FewestNodes
 *
 * @author dwalend
 * @since v0.1.0
 */

class FewestNodesTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-arcs and arcs where SomeGraph has them" in {

    val labelGraph = FloydWarshall.createLabelDigraph(testGraph.edges,testGraph.nodesSeq,FewestNodes,FewestNodes.convertEdgeToLabel)

    val expectedArcs = Set(
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

    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  val expectedArcs = Set(
    (A,A,0),
    (A,B,1),
    (A,C,2),
    (A,D,3),
    (A,E,4),
    (A,F,5),
    (A,H,5),
    (B,B,0),
    (B,C,1),
    (B,D,2),
    (B,E,3),
    (B,F,4),
    (B,H,4),
    (C,B,3),
    (C,C,0),
    (C,D,1),
    (C,E,2),
    (C,F,3),
    (C,H,3),
    (D,B,2),
    (D,C,3),
    (D,D,0),
    (D,E,1),
    (D,F,2),
    (D,H,2),
    (E,B,1),
    (E,C,2),
    (E,D,3),
    (E,E,0),
    (E,F,1),
    (E,H,1),
    (F,F,0),
    (G,G,0),
    (H,B,4),
    (H,C,1),
    (H,D,2),
    (H,E,3),
    (H,F,4),
    (H,H,0)
  )

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodesSeq,FewestNodes,FewestNodes.convertEdgeToLabel)

    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(testGraph.edges,testGraph.nodesSeq,FewestNodes,FewestNodes.convertEdgeToLabel)

    labels.size should be (expectedArcs.size)
    labels.to[Set] should be (expectedArcs)
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

  "Brandes' algorithm" should "produce both the correct label graph and betweenness for Somegraph" in {

    val brandesSupport = new Brandes.BrandesSupport[String,Int,Int](FewestNodes)

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testGraph.edges,testGraph.nodesSeq,brandesSupport,FewestNodes.convertEdgeToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }
}
