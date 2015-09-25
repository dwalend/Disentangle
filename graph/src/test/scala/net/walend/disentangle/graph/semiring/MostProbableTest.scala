package net.walend.disentangle.graph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.disentangle.graph.SomeGraph
import SomeGraph._

/**
 * Tests algorithms with MostProbable
 *
 * @author dwalend
 * @since v0.1.0
 */

class MostProbableTest extends FlatSpec with Matchers {

  val arcsToWeights:Map[(String,String,String),Double] = Map(ab -> 1.0
                                                              ,bc -> 0.9
                                                              ,cd -> 0.8
                                                              ,de -> 0.7
                                                              ,ef -> 0.6
                                                              ,eb -> 0.5
                                                              ,eh -> 0.4
                                                              ,hc -> 0.3
                                                            )

  def convertEdgeToLabel(start: String, end: String, edge: String): LeastWeights.Label = arcsToWeights.get((start,end,edge)).get


  val expectedArcs = Set(
                          (A,A,1.0),
                          (A,B,1.0),
                          (A,C,0.9),
                          (A,D,0.7200000000000001),
                          (A,E,0.504),
                          (A,F,0.3024),
                          (A,H,0.2016),
                          (B,B,1.0),
                          (B,C,0.9),
                          (B,D,0.7200000000000001),
                          (B,E,0.504),
                          (B,F,0.3024),
                          (B,H,0.2016),
                          (C,B,0.27999999999999997),
                          (C,C,1.0),
                          (C,D,0.8),
                          (C,E,0.5599999999999999),
                          (C,F,0.33599999999999997),
                          (C,H,0.22399999999999998),
                          (D,B,0.35),
                          (D,C,0.315),
                          (D,D,1.0),
                          (D,E,0.7),
                          (D,F,0.42),
                          (D,H,0.27999999999999997),
                          (E,B,0.5),
                          (E,C,0.45),
                          (E,D,0.36000000000000004),
                          (E,E,1.0),
                          (E,F,0.6),
                          (E,H,0.4),
                          (F,F,1.0),
                          (G,G,1.0),
                          (H,B,0.08399999999999999),
                          (H,C,0.3),
                          (H,D,0.24),
                          (H,E,0.16799999999999998),
                          (H,F,0.10079999999999999),
                          (H,H,1.0)
                        )


  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodes.to[Seq],MostProbable,convertEdgeToLabel)

    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val edges = Dijkstra.allPairsShortestPaths(testGraph.edges,testGraph.nodes.to[Seq],MostProbable,convertEdgeToLabel)

    edges.to[Set] -- expectedArcs should be (Set.empty)
    expectedArcs -- edges.to[Set] should be (Set.empty)
    edges.to[Set] should be (expectedArcs)
  }

  val expectedBetweenness:Map[String,Double] = Map(
    A -> 0.0,
    B -> 8.0,
    C -> 13.0,
    D -> 13.0,
    E -> 13.0,
    F -> 0.0,
    G -> 0.0,
    H -> 0.0
  )

  "Brandes' algorithm" should "produce both the correct label graph and betweenness for Somegraph" in {

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testGraph.edges,testGraph.nodes.to[Seq],MostProbable,convertEdgeToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }

}
