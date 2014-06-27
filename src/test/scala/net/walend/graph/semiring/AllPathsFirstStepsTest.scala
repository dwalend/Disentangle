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

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  val expectedArcs = Set[(String,String,Option[FirstStepsTrait[String,Int]])](
    (A,A,Some(support.FirstSteps(0,Seq()))),
    (A,B,Some(support.FirstSteps(1,Seq(B)))),
    (A,C,Some(support.FirstSteps(2,Seq(B)))),
    (A,D,Some(support.FirstSteps(3,Seq(B)))),
    (A,E,Some(support.FirstSteps(4,Seq(B)))),
    (A,F,Some(support.FirstSteps(5,Seq(B)))),
    (A,H,Some(support.FirstSteps(5,Seq(B)))),
    (B,B,Some(support.FirstSteps(0,Seq()))),
    (B,C,Some(support.FirstSteps(1,Seq(C)))),
    (B,D,Some(support.FirstSteps(2,Seq(C)))),
    (B,E,Some(support.FirstSteps(3,Seq(C)))),
    (B,F,Some(support.FirstSteps(4,Seq(C)))),
    (B,H,Some(support.FirstSteps(4,Seq(C)))),
    (C,B,Some(support.FirstSteps(3,Seq(D)))),
    (C,C,Some(support.FirstSteps(0,Seq()))),
    (C,D,Some(support.FirstSteps(1,Seq(D)))),
    (C,E,Some(support.FirstSteps(2,Seq(D)))),
    (C,F,Some(support.FirstSteps(3,Seq(D)))),
    (C,H,Some(support.FirstSteps(3,Seq(D)))),
    (D,B,Some(support.FirstSteps(2,Seq(E)))),
    (D,C,Some(support.FirstSteps(3,Seq(E)))),
    (D,D,Some(support.FirstSteps(0,Seq()))),
    (D,E,Some(support.FirstSteps(1,Seq(E)))),
    (D,F,Some(support.FirstSteps(2,Seq(E)))),
    (D,H,Some(support.FirstSteps(2,Seq(E)))),
    (E,B,Some(support.FirstSteps(1,Seq(B)))),
    (E,C,Some(support.FirstSteps(2,Seq(B, H)))),
    (E,D,Some(support.FirstSteps(3,Seq(B, H)))),
    (E,E,Some(support.FirstSteps(0,Seq()))),
    (E,F,Some(support.FirstSteps(1,Seq(F)))),
    (E,H,Some(support.FirstSteps(1,Seq(H)))),
    (F,F,Some(support.FirstSteps(0,Seq()))),
    (G,G,Some(support.FirstSteps(0,Seq()))),
    (H,B,Some(support.FirstSteps(4,Seq(C)))),
    (H,C,Some(support.FirstSteps(1,Seq(C)))),
    (H,D,Some(support.FirstSteps(2,Seq(C)))),
    (H,E,Some(support.FirstSteps(3,Seq(C)))),
    (H,F,Some(support.FirstSteps(4,Seq(C)))),
    (H,H,Some(support.FirstSteps(0,Seq())))
  )

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
