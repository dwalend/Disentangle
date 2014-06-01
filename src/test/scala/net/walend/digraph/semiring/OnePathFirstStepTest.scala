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
    (A,A,Some(FirstStep(0,None))),
    (A,B,Some(FirstStep(1,Some(B)))),
    (A,C,Some(FirstStep(2,Some(B)))),
    (A,D,Some(FirstStep(3,Some(B)))),
    (A,E,Some(FirstStep(4,Some(B)))),
    (A,F,Some(FirstStep(5,Some(B)))),
    (A,H,Some(FirstStep(5,Some(B)))),
    (B,B,Some(FirstStep(0,None))),
    (B,C,Some(FirstStep(1,Some(C)))),
    (B,D,Some(FirstStep(2,Some(C)))),
    (B,E,Some(FirstStep(3,Some(C)))),
    (B,F,Some(FirstStep(4,Some(C)))),
    (B,H,Some(FirstStep(4,Some(C)))),
    (C,B,Some(FirstStep(3,Some(D)))),
    (C,C,Some(FirstStep(0,None))),
    (C,D,Some(FirstStep(1,Some(D)))),
    (C,E,Some(FirstStep(2,Some(D)))),
    (C,F,Some(FirstStep(3,Some(D)))),
    (C,H,Some(FirstStep(3,Some(D)))),
    (D,B,Some(FirstStep(2,Some(E)))),
    (D,C,Some(FirstStep(3,Some(E)))),
    (D,D,Some(FirstStep(0,None))),
    (D,E,Some(FirstStep(1,Some(E)))),
    (D,F,Some(FirstStep(2,Some(E)))),
    (D,H,Some(FirstStep(2,Some(E)))),
    (E,B,Some(FirstStep(1,Some(B)))),
    (E,C,Some(FirstStep(2,Some(B)))), //
    (E,D,Some(FirstStep(3,Some(B)))), //
    (E,E,Some(FirstStep(0,None))),
    (E,F,Some(FirstStep(1,Some(F)))),
    (E,H,Some(FirstStep(1,Some(H)))),
    (F,F,Some(FirstStep(0,None))),
    (G,G,Some(FirstStep(0,None))),
    (H,B,Some(FirstStep(4,Some(C)))),
    (H,C,Some(FirstStep(1,Some(C)))),
    (H,D,Some(FirstStep(2,Some(C)))),
    (H,E,Some(FirstStep(3,Some(C)))),
    (H,F,Some(FirstStep(4,Some(C)))),
    (H,H,Some(FirstStep(0,None)))
  )

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
