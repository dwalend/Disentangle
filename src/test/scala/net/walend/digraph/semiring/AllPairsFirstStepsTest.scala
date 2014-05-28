package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._
import net.walend.digraph.{IndexedDigraph, MutableEdgeDigraph}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class AllPairsFirstStepsTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (B,B,Some(FirstSteps(0,Set()))),
    (B,F,Some(FirstSteps(4,Set(C)))),
    (B,C,Some(FirstSteps(1,Set(C)))),
    (B,D,Some(FirstSteps(2,Set(C)))),
    (B,H,Some(FirstSteps(4,Set(C)))),
    (B,E,Some(FirstSteps(3,Set(C)))),
    (F,F,Some(FirstSteps(0,Set()))),
    (C,B,Some(FirstSteps(3,Set(D)))),
    (C,F,Some(FirstSteps(3,Set(D)))),
    (C,C,Some(FirstSteps(0,Set()))),
    (C,D,Some(FirstSteps(1,Set(D)))),
    (C,H,Some(FirstSteps(3,Set(D)))),
    (C,E,Some(FirstSteps(2,Set(D)))),
    (G,G,Some(FirstSteps(0,Set()))),
    (D,B,Some(FirstSteps(2,Set(E)))),
    (D,F,Some(FirstSteps(2,Set(E)))),
    (D,C,Some(FirstSteps(3,Set(E)))),
    (D,D,Some(FirstSteps(0,Set()))),
    (D,H,Some(FirstSteps(2,Set(E)))),
    (D,E,Some(FirstSteps(1,Set(E)))),
    (H,B,Some(FirstSteps(4,Set(C)))),
    (H,F,Some(FirstSteps(4,Set(C)))),
    (H,C,Some(FirstSteps(1,Set(C)))),
    (H,D,Some(FirstSteps(2,Set(C)))),
    (H,H,Some(FirstSteps(0,Set()))),
    (H,E,Some(FirstSteps(3,Set(C)))),
    (E,B,Some(FirstSteps(1,Set(B)))),
    (E,F,Some(FirstSteps(1,Set(F)))),
    (E,C,Some(FirstSteps(2,Set(B, H)))),
    (E,D,Some(FirstSteps(3,Set(B, H)))),
    (E,H,Some(FirstSteps(1,Set(H)))),
    (E,E,Some(FirstSteps(0,Set()))),
    (A,B,Some(FirstSteps(1,Set(B)))),
    (A,F,Some(FirstSteps(5,Set(B)))),
    (A,C,Some(FirstSteps(2,Set(B)))),
    (A,D,Some(FirstSteps(3,Set(B)))),
    (A,H,Some(FirstSteps(5,Set(B)))),
    (A,E,Some(FirstSteps(4,Set(B)))),
    (A,A,Some(FirstSteps(0,Set()))))

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val initialGraph:MutableEdgeDigraph[String,support.Label] = FloydWarshall.convert(testGraph,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))
    val labelGraph = FloydWarshall.allPairsShortestPaths(initialGraph,support)

    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val initialGraph:IndexedDigraph[String,support.Label] = Dijkstra.convert(testGraph,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))
    val labels = Dijkstra.allPairsShortestPaths(initialGraph,support)

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
  }


  val expectedBetweenness:Map[String,Double] = Map(E -> 13.0,
    F -> 0.0,
    A -> 0.0,
    G -> 0.0,
    B -> 6.5,
    C -> 13.0,
    H -> 1.5,
    D -> 13.0)


  "Brandes' algorithm" should "produce both the correct label graph and betweenness for Somegraph" in {

    val initialGraph:IndexedDigraph[String,support.Label] = Dijkstra.convert(testGraph,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))
    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(initialGraph,support)

    labelGraphAndBetweenness._1.to[Set] should be (expectedEdges)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }
}
