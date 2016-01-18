package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.SomeGraph
import org.scalatest.{Matchers, FlatSpec}

/**
  *
  *
  * @author dwalend
  * @since v0.2.1
  */
class UndirectedGraphTest extends FlatSpec with Matchers {

  import SomeGraph._

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  val expectedShortestPaths = Vector(
    (A,A,Some(support.FirstSteps(0,Set()))),
    (A,B,Some(support.FirstSteps(1,Set(B)))),
    (A,C,Some(support.FirstSteps(2,Set(B)))),
    (A,D,Some(support.FirstSteps(3,Set(B)))),
    (A,E,Some(support.FirstSteps(2,Set(B)))),
    (A,F,Some(support.FirstSteps(3,Set(B)))),
    (A,H,Some(support.FirstSteps(3,Set(B)))),
    (B,A,Some(support.FirstSteps(1,Set(A)))),
    (B,B,Some(support.FirstSteps(0,Set()))),
    (B,C,Some(support.FirstSteps(1,Set(C)))),
    (B,D,Some(support.FirstSteps(2,Set(C, E)))),
    (B,E,Some(support.FirstSteps(1,Set(E)))),
    (B,F,Some(support.FirstSteps(2,Set(E)))),
    (B,H,Some(support.FirstSteps(2,Set(C, E)))),
    (C,A,Some(support.FirstSteps(2,Set(B)))),
    (C,B,Some(support.FirstSteps(1,Set(B)))),
    (C,C,Some(support.FirstSteps(0,Set()))),
    (C,D,Some(support.FirstSteps(1,Set(D)))),
    (C,E,Some(support.FirstSteps(2,Set(D, B, H)))),
    (C,F,Some(support.FirstSteps(3,Set(D, B, H)))),
    (C,H,Some(support.FirstSteps(1,Set(H)))),
    (D,A,Some(support.FirstSteps(3,Set(E, C)))),
    (D,B,Some(support.FirstSteps(2,Set(E, C)))),
    (D,C,Some(support.FirstSteps(1,Set(C)))),
    (D,D,Some(support.FirstSteps(0,Set()))),
    (D,E,Some(support.FirstSteps(1,Set(E)))),
    (D,F,Some(support.FirstSteps(2,Set(E)))),
    (D,H,Some(support.FirstSteps(2,Set(E, C)))),
    (E,A,Some(support.FirstSteps(2,Set(B)))),
    (E,B,Some(support.FirstSteps(1,Set(B)))),
    (E,C,Some(support.FirstSteps(2,Set(B, H, D)))),
    (E,D,Some(support.FirstSteps(1,Set(D)))),
    (E,E,Some(support.FirstSteps(0,Set()))),
    (E,F,Some(support.FirstSteps(1,Set(F)))),
    (E,H,Some(support.FirstSteps(1,Set(H)))),
    (F,A,Some(support.FirstSteps(3,Set(E)))),
    (F,B,Some(support.FirstSteps(2,Set(E)))),
    (F,C,Some(support.FirstSteps(3,Set(E)))),
    (F,D,Some(support.FirstSteps(2,Set(E)))),
    (F,E,Some(support.FirstSteps(1,Set(E)))),
    (F,F,Some(support.FirstSteps(0,Set()))),
    (F,H,Some(support.FirstSteps(2,Set(E)))),
    (G,G,Some(support.FirstSteps(0,Set()))),
    (H,A,Some(support.FirstSteps(3,Set(C, E)))),
    (H,B,Some(support.FirstSteps(2,Set(C, E)))),
    (H,C,Some(support.FirstSteps(1,Set(C)))),
    (H,D,Some(support.FirstSteps(2,Set(C, E)))),
    (H,E,Some(support.FirstSteps(1,Set(E)))),
    (H,F,Some(support.FirstSteps(2,Set(E)))),
    (H,H,Some(support.FirstSteps(0,Set())))
  )

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val allShortestPaths = SomeGraph.testUndigraph.allPairsShortestPaths

    allShortestPaths should be(expectedShortestPaths)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph in parallel" in {

    val allShortestPaths = SomeGraph.testUndigraph.parAllPairsShortestPaths

    allShortestPaths should be(expectedShortestPaths)
  }

}
