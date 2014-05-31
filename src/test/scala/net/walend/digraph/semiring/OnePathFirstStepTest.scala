package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class OnePathFirstStepTest extends FlatSpec with Matchers {

  val expectedEdges = Set[(String, String, Option[net.walend.digraph.semiring.FirstStep[String,Int]])](
    (B,B,Some(FirstStep(0,None))),
    (B,F,Some(FirstStep(4,Some(C)))),
    (B,C,Some(FirstStep(1,Some(C)))),
    (B,D,Some(FirstStep(2,Some(C)))),
    (B,H,Some(FirstStep(4,Some(C)))),
    (B,E,Some(FirstStep(3,Some(C)))),
    (F,F,Some(FirstStep(0,None))),
    (C,B,Some(FirstStep(3,Some(D)))),
    (C,F,Some(FirstStep(3,Some(D)))),
    (C,C,Some(FirstStep(0,None))),
    (C,D,Some(FirstStep(1,Some(D)))),
    (C,H,Some(FirstStep(3,Some(D)))),
    (C,E,Some(FirstStep(2,Some(D)))),
    (G,G,Some(FirstStep(0,None))),
    (D,B,Some(FirstStep(2,Some(E)))),
    (D,F,Some(FirstStep(2,Some(E)))),
    (D,C,Some(FirstStep(3,Some(E)))),
    (D,D,Some(FirstStep(0,None))),
    (D,H,Some(FirstStep(2,Some(E)))),
    (D,E,Some(FirstStep(1,Some(E)))),
    (H,B,Some(FirstStep(4,Some(C)))),
    (H,F,Some(FirstStep(4,Some(C)))),
    (H,C,Some(FirstStep(1,Some(C)))),
    (H,D,Some(FirstStep(2,Some(C)))),
    (H,H,Some(FirstStep(0,None))),
    (H,E,Some(FirstStep(3,Some(C)))),
    (E,B,Some(FirstStep(1,Some(B)))),
    (E,F,Some(FirstStep(1,Some(F)))),
    (E,C,Some(FirstStep(2,Some(B)))), //
    (E,D,Some(FirstStep(3,Some(B)))), //
    (E,H,Some(FirstStep(1,Some(H)))),
    (E,E,Some(FirstStep(0,None))),
    (A,B,Some(FirstStep(1,Some(B)))),
    (A,F,Some(FirstStep(5,Some(B)))),
    (A,C,Some(FirstStep(2,Some(B)))),
    (A,D,Some(FirstStep(3,Some(B)))),
    (A,H,Some(FirstStep(5,Some(B)))),
    (A,E,Some(FirstStep(4,Some(B)))),
    (A,A,Some(FirstStep(0,None))))

  val support = new OnePathFirstStep[String,Int,Int](FewestNodes)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodes,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    labelGraph.edges.to[Set] -- expectedEdges should be (Set.empty)
    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(testGraph.edges,testGraph.nodes,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
  }
}
