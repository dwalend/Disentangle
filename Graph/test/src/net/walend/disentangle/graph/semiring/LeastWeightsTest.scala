package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.SomeGraph
import SomeGraph._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tests algorithms with LeastWeights
 *
 * @author dwalend
 * @since v0.1.0
 */

class LeastWeightsTest extends AnyFlatSpec with Matchers {

  val arcsToWeights:Map[(String,String,String),Double] = Map(ab -> 1.0
                                                              ,bc -> 2.0
                                                              ,cd -> 3.0
                                                              ,de -> 4.0
                                                              ,ef -> 5.0
                                                              ,eb -> 6.0
                                                              ,eh -> 7.0
                                                              ,hc -> 8.0
                                                              )

  def convertArcToLabel(start: String, end: String, arc: String): LeastWeights.Label = arcsToWeights.get((start,end,arc)).get


  val expectedArcs = Set(
                          (A,A,0.0),
                          (A,B,1.0),
                          (A,C,3.0),
                          (A,D,6.0),
                          (A,E,10.0),
                          (A,F,15.0),
                          (A,H,17.0),
                          (B,B,0.0),
                          (B,C,2.0),
                          (B,D,5.0),
                          (B,E,9.0),
                          (B,F,14.0),
                          (B,H,16.0),
                          (C,B,13.0),
                          (C,C,0.0),
                          (C,D,3.0),
                          (C,E,7.0),
                          (C,F,12.0),
                          (C,H,14.0),
                          (D,B,10.0),
                          (D,C,12.0),
                          (D,D,0.0),
                          (D,E,4.0),
                          (D,F,9.0),
                          (D,H,11.0),
                          (E,B,6.0),
                          (E,C,8.0),
                          (E,D,11.0),
                          (E,E,0.0),
                          (E,F,5.0),
                          (E,H,7.0),
                          (F,F,0.0),
                          (G,G,0.0),
                          (H,B,21.0),
                          (H,C,8.0),
                          (H,D,11.0),
                          (H,E,15.0),
                          (H,F,20.0),
                          (H,H,0.0)
                        )


  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsLeastPaths(testDigraph.edges,testDigraph.nodes.to[Seq],LeastWeights,convertArcToLabel)

    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val edges = Dijkstra.allPairsLeastPaths(testDigraph.edges,LeastWeights,convertArcToLabel,testDigraph.nodes.to[Seq])

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

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testDigraph.edges,testDigraph.nodes.to[Seq],LeastWeights,convertArcToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }
}
