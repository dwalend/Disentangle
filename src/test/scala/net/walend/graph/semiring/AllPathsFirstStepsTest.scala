package net.walend.graph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.graph.SomeGraph._

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class AllPathsFirstStepsTest extends FlatSpec with Matchers {

  val expectedArcs = Set[(String,String,Option[FirstSteps[String,Int]])](
    (A,A,Some(FirstSteps(0,Seq()))),
    (A,B,Some(FirstSteps(1,Seq(B)))),
    (A,C,Some(FirstSteps(2,Seq(B)))),
    (A,D,Some(FirstSteps(3,Seq(B)))),
    (A,E,Some(FirstSteps(4,Seq(B)))),
    (A,F,Some(FirstSteps(5,Seq(B)))),
    (A,H,Some(FirstSteps(5,Seq(B)))),
    (B,B,Some(FirstSteps(0,Seq()))),
    (B,C,Some(FirstSteps(1,Seq(C)))),
    (B,D,Some(FirstSteps(2,Seq(C)))),
    (B,E,Some(FirstSteps(3,Seq(C)))),
    (B,F,Some(FirstSteps(4,Seq(C)))),
    (B,H,Some(FirstSteps(4,Seq(C)))),
    (C,B,Some(FirstSteps(3,Seq(D)))),
    (C,C,Some(FirstSteps(0,Seq()))),
    (C,D,Some(FirstSteps(1,Seq(D)))),
    (C,E,Some(FirstSteps(2,Seq(D)))),
    (C,F,Some(FirstSteps(3,Seq(D)))),
    (C,H,Some(FirstSteps(3,Seq(D)))),
    (D,B,Some(FirstSteps(2,Seq(E)))),
    (D,C,Some(FirstSteps(3,Seq(E)))),
    (D,D,Some(FirstSteps(0,Seq()))),
    (D,E,Some(FirstSteps(1,Seq(E)))),
    (D,F,Some(FirstSteps(2,Seq(E)))),
    (D,H,Some(FirstSteps(2,Seq(E)))),
    (E,B,Some(FirstSteps(1,Seq(B)))),
    (E,C,Some(FirstSteps(2,Seq(B, H)))),
    (E,D,Some(FirstSteps(3,Seq(B, H)))),
    (E,E,Some(FirstSteps(0,Seq()))),
    (E,F,Some(FirstSteps(1,Seq(F)))),
    (E,H,Some(FirstSteps(1,Seq(H)))),
    (F,F,Some(FirstSteps(0,Seq()))),
    (G,G,Some(FirstSteps(0,Seq()))),
    (H,B,Some(FirstSteps(4,Seq(C)))),
    (H,C,Some(FirstSteps(1,Seq(C)))),
    (H,D,Some(FirstSteps(2,Seq(C)))),
    (H,E,Some(FirstSteps(3,Seq(C)))),
    (H,F,Some(FirstSteps(4,Seq(C)))),
    (H,H,Some(FirstSteps(0,Seq())))
  )

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodesSeq,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val arcs = Dijkstra.allPairsShortestPaths(testGraph.edges,testGraph.nodesSeq,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    arcs.size should be (expectedArcs.size)
    arcs.to[Set] should be (expectedArcs)
  }
}
