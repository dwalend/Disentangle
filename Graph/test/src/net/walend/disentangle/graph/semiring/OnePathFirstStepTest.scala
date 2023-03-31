package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.SomeGraph
import SomeGraph._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class OnePathFirstStepTest extends AnyFlatSpec with Matchers {

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

    val labelGraph = FloydWarshall.allPairsLeastPaths(testDigraph.edges,Seq.from(testDigraph.nodes),support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    Set.from(labelGraph.edges) -- expectedArcs should be (Set.empty)
    Set.from(labelGraph.edges) should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labelTuples:Seq[(String,String,Option[FirstStepTrait[String,Int]])] = Dijkstra.allPairsLeastPaths(edges = testDigraph.edges, support = support, labelForEdge = support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel), nodeOrder = Seq.from(testDigraph.nodes))

    labelTuples.size should be (expectedArcs.size)
    Set.from(labelTuples) should be (expectedArcs)
  }

  "OnePathFirstStep and FloydWarshall's algorithm" should "produce labels that can create the correct shortest paths for Somegraph" in {
    
    val expectedPaths = Map(
      (A,A) -> Some(List()),
      (A,B) -> Some(List(B)),
      (A,C) -> Some(List(B, C)),
      (A,D) -> Some(List(B, C, D)),
      (A,E) -> Some(List(B, C, D, E)),
      (A,F) -> Some(List(B, C, D, E, F)),
      (A,H) -> Some(List(B, C, D, E, H)),
      (B,B) -> Some(List()),
      (B,C) -> Some(List(C)),
      (B,D) -> Some(List(C, D)),
      (B,E) -> Some(List(C, D, E)),
      (B,F) -> Some(List(C, D, E, F)),
      (B,H) -> Some(List(C, D, E, H)),
      (C,B) -> Some(List(D, E, B)),
      (C,C) -> Some(List()),
      (C,D) -> Some(List(D)),
      (C,E) -> Some(List(D, E)),
      (C,F) -> Some(List(D, E, F)),
      (C,H) -> Some(List(D, E, H)),
      (D,B) -> Some(List(E, B)),
      (D,C) -> Some(List(E, B, C)),
      (D,D) -> Some(List()),
      (D,E) -> Some(List(E)),
      (D,F) -> Some(List(E, F)),
      (D,H) -> Some(List(E, H)),
      (E,B) -> Some(List(B)),
      (E,C) -> Some(List(B, C)),
      (E,D) -> Some(List(B, C, D)),
      (E,E) -> Some(List()),
      (E,F) -> Some(List(F)),
      (E,H) -> Some(List(H)),
      (F,F) -> Some(List()),
      (G,G) -> Some(List()),
      (H,B) -> Some(List(C, D, E, B)),
      (H,C) -> Some(List(C)),
      (H,D) -> Some(List(C, D)),
      (H,E) -> Some(List(C, D, E)),
      (H,F) -> Some(List(C, D, E, F)),
      (H,H) -> Some(List())
    )
    
    val labelGraph = FloydWarshall.allPairsLeastPaths(testDigraph.edges,Seq.from(testDigraph.nodes),support,support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel))

    val pairsToPathsOfInnerNodes = labelGraph.edges.map(edge => ((edge._1,edge._2),support.leastPath(edge._1,edge._2)(labelGraph)))

    val pairsToPaths = pairsToPathsOfInnerNodes.map(x => (x._1,Some(x._2.get.map(node => node.value)))).toMap

    pairsToPaths should be (expectedPaths)
  }
}
