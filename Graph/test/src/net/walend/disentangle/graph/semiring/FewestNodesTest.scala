package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.SomeGraph
import SomeGraph._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Tests algorithms with FewestNodes
 *
 * @author dwalend
 * @since v0.1.0
 */

class FewestNodesTest extends AnyFlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-arcs and arcs where SomeGraph has them" in {

    val labelGraph = FloydWarshall.createLabelDigraph(testDigraph.edges,Seq.from(testDigraph.nodes),FewestNodes,FewestNodes.convertEdgeToLabel)

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

    Set.from(labelGraph.edges) should be (expectedArcs)
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

    val labelGraph = FloydWarshall.allPairsLeastPaths(testDigraph.edges,Seq.from(testDigraph.nodes),FewestNodes,FewestNodes.convertEdgeToLabel)

    Set.from(labelGraph.edges) should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsLeastPaths(testDigraph.edges, FewestNodes, FewestNodes.convertEdgeToLabel, Seq.from(testDigraph.nodes))

    labels.size should be (expectedArcs.size)
    Set.from(labels) should be (expectedArcs)
  }
/*
  "Parallel Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.parAllPairsLeastPaths(testDigraph.edges, FewestNodes, FewestNodes.convertEdgeToLabel, testDigraph.nodes.to[Seq])

    labels.size should be (expectedArcs.size)
    labels.to[Set] should be (expectedArcs)
  }
*/
  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph using the implicit method on testDigraph" in {

    val labels = testDigraph.allPairsLeastPaths(FewestNodes, FewestNodes.convertEdgeToLabel)

    labels.size should be (expectedArcs.size)
    Set.from(labels) should be (expectedArcs)
  }
/*
  "Parallel Dijkstra's algorithm" should "produce the correct label graph for Somegraph using the implicit method on testDigraph" in {

    val labels = testDigraph.parAllPairsLeastPaths(FewestNodes, FewestNodes.convertEdgeToLabel)

    labels.size should be (expectedArcs.size)
    labels.to[Set] should be (expectedArcs)
  }
*/


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

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testDigraph.edges,Seq.from(testDigraph.nodes),FewestNodes,FewestNodes.convertEdgeToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }
/*
  "Parallel Dijkstra for Enron data" should "be calculated" in {

    import scala.io.Source
    import scala.pickling._
    import scala.pickling.json._

    val support = FewestNodes

    val fileContents = Source.fromURL(getClass.getResource("/Enron2000Apr.json")).mkString
    val edges = JSONPickle(fileContents).unpickle[Seq[(String,String,Int)]]

    val labels = Dijkstra.parAllPairsLeastPaths(edges, FewestNodes, FewestNodes.convertEdgeToLabel, Seq.empty)
  }
*/
}
