package net.walend.graph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.graph.SomeGraph._

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class OnePathFirstStepTest extends FlatSpec with Matchers {

  val support = new OnePathFirstStep[String,Int,Int](FewestNodes)

  val expectedArcs = Set[(String, String, Option[FirstStepTrait[String,Int]])](
    (A,A,Some(support.FirstStep(0,None))),
    (A,B,Some(support.FirstStep(1,Some(B)))),
    (A,C,Some(support.FirstStep(2,Some(B)))),
    (A,D,Some(support.FirstStep(3,Some(B)))),
    (A,E,Some(support.FirstStep(4,Some(B)))),
    (A,F,Some(support.FirstStep(5,Some(B)))),
    (A,H,Some(support.FirstStep(5,Some(B)))),
    (B,B,Some(support.FirstStep(0,None))),
    (B,C,Some(support.FirstStep(1,Some(C)))),
    (B,D,Some(support.FirstStep(2,Some(C)))),
    (B,E,Some(support.FirstStep(3,Some(C)))),
    (B,F,Some(support.FirstStep(4,Some(C)))),
    (B,H,Some(support.FirstStep(4,Some(C)))),
    (C,B,Some(support.FirstStep(3,Some(D)))),
    (C,C,Some(support.FirstStep(0,None))),
    (C,D,Some(support.FirstStep(1,Some(D)))),
    (C,E,Some(support.FirstStep(2,Some(D)))),
    (C,F,Some(support.FirstStep(3,Some(D)))),
    (C,H,Some(support.FirstStep(3,Some(D)))),
    (D,B,Some(support.FirstStep(2,Some(E)))),
    (D,C,Some(support.FirstStep(3,Some(E)))),
    (D,D,Some(support.FirstStep(0,None))),
    (D,E,Some(support.FirstStep(1,Some(E)))),
    (D,F,Some(support.FirstStep(2,Some(E)))),
    (D,H,Some(support.FirstStep(2,Some(E)))),
    (E,B,Some(support.FirstStep(1,Some(B)))),
    (E,C,Some(support.FirstStep(2,Some(B)))), //
    (E,D,Some(support.FirstStep(3,Some(B)))), //
    (E,E,Some(support.FirstStep(0,None))),
    (E,F,Some(support.FirstStep(1,Some(F)))),
    (E,H,Some(support.FirstStep(1,Some(H)))),
    (F,F,Some(support.FirstStep(0,None))),
    (G,G,Some(support.FirstStep(0,None))),
    (H,B,Some(support.FirstStep(4,Some(C)))),
    (H,C,Some(support.FirstStep(1,Some(C)))),
    (H,D,Some(support.FirstStep(2,Some(C)))),
    (H,E,Some(support.FirstStep(3,Some(C)))),
    (H,F,Some(support.FirstStep(4,Some(C)))),
    (H,H,Some(support.FirstStep(0,None)))
  )

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.edges,testGraph.nodesSeq,support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    labelGraph.edges.to[Set] -- expectedArcs should be (Set.empty)
    labelGraph.edges.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labelTuples:Seq[(String,String,Option[FirstStepTrait[String,Int]])] = Dijkstra.allPairsShortestPaths(
                                                edges = testGraph.edges,
                                                extraNodes = testGraph.nodesSeq,
                                                support = support,
                                                labelForEdge = support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    labelTuples.size should be (expectedArcs.size)
    labelTuples.to[Set] should be (expectedArcs)
  }
}
