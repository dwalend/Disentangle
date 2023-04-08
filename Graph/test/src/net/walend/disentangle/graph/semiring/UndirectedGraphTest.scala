package net.walend.disentangle.graph.semiring

import munit.FunSuite
import net.walend.disentangle.graph.SomeGraph
import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps

/**
  *
  *
  * @author dwalend
  * @since v0.2.1
  */
class UndirectedGraphTest extends FunSuite {

  import SomeGraph._

  val support: AllPathsFirstSteps[String, Int, Int] = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

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

  test("Dijkstra's algorithm should produce the correct label graph for Somegraph") {

    val allShortestPaths: Seq[(String, String, Option[FirstStepsTrait[String, Int]])] = SomeGraph.testLabelUndigraph.allPairsShortestPaths

    assertEquals(allShortestPaths, expectedShortestPaths)
  }
  
  test("Brandes algorithm should produce the correct label graph and betweeness values for Somegraph") {

    val expectedFirstSteps: Seq[(String, String, Option[BrandesSteps[String, Int]])] = Vector((A,A,Some(BrandesSteps(0,1,List()))),
      (B,A,Some(BrandesSteps(1,1,List(0)))),
      (E,A,Some(BrandesSteps(2,1,List(1)))),
      (C,A,Some(BrandesSteps(2,1,List(1)))),
      (H,A,Some(BrandesSteps(3,2,List(2, 3)))),
      (D,A,Some(BrandesSteps(3,2,List(2, 3)))),
      (F,A,Some(BrandesSteps(3,1,List(2)))),
      (A,B,Some(BrandesSteps(1,1,List(1)))),
      (B,B,Some(BrandesSteps(0,1,List()))),
      (E,B,Some(BrandesSteps(1,1,List(1)))),
      (C,B,Some(BrandesSteps(1,1,List(1)))),
      (H,B,Some(BrandesSteps(2,2,List(2, 3)))),
      (D,B,Some(BrandesSteps(2,2,List(2, 3)))),
      (F,B,Some(BrandesSteps(2,1,List(2)))),
      (A,E,Some(BrandesSteps(2,1,List(1)))),
      (B,E,Some(BrandesSteps(1,1,List(2)))),
      (E,E,Some(BrandesSteps(0,1,List()))),
      (C,E,Some(BrandesSteps(2,3,List(5, 1, 4)))),
      (H,E,Some(BrandesSteps(1,1,List(2)))),
      (D,E,Some(BrandesSteps(1,1,List(2)))),
      (F,E,Some(BrandesSteps(1,1,List(2)))),
      (A,C,Some(BrandesSteps(2,1,List(1)))),
      (B,C,Some(BrandesSteps(1,1,List(3)))),
      (E,C,Some(BrandesSteps(2,3,List(1, 4, 5)))),
      (C,C,Some(BrandesSteps(0,1,List()))),
      (H,C,Some(BrandesSteps(1,1,List(3)))),
      (D,C,Some(BrandesSteps(1,1,List(3)))),
      (F,C,Some(BrandesSteps(3,3,List(2)))),
      (A,H,Some(BrandesSteps(3,2,List(1)))),
      (B,H,Some(BrandesSteps(2,2,List(2, 3)))),
      (E,H,Some(BrandesSteps(1,1,List(4)))),
      (C,H,Some(BrandesSteps(1,1,List(4)))),
      (H,H,Some(BrandesSteps(0,1,List()))),
      (D,H,Some(BrandesSteps(2,2,List(2, 3)))),
      (F,H,Some(BrandesSteps(2,1,List(2)))),
      (A,D,Some(BrandesSteps(3,2,List(1)))),
      (B,D,Some(BrandesSteps(2,2,List(3, 2)))),
      (E,D,Some(BrandesSteps(1,1,List(5)))),
      (C,D,Some(BrandesSteps(1,1,List(5)))),
      (H,D,Some(BrandesSteps(2,2,List(3, 2)))),
      (D,D,Some(BrandesSteps(0,1,List()))),
      (F,D,Some(BrandesSteps(2,1,List(2)))),
      (A,F,Some(BrandesSteps(3,1,List(1)))),
      (B,F,Some(BrandesSteps(2,1,List(2)))),
      (E,F,Some(BrandesSteps(1,1,List(6)))),
      (C,F,Some(BrandesSteps(3,3,List(5, 1, 4)))),
      (H,F,Some(BrandesSteps(2,1,List(2)))),
      (D,F,Some(BrandesSteps(2,1,List(2)))),
      (F,F,Some(BrandesSteps(0,1,List()))))

    val expectedBetweennesses = Map(
      A -> 0.0,
      B -> 5.666666666666667,
      C -> 2.5,
      D -> 0.6666666666666666,
      E -> 7.5,
      F -> 0.0,
      H -> 0.6666666666666666
    )

    val allShortestPathsAndBetweenesses: (Seq[(String, String, Option[BrandesSteps[String, Int]])], Map[String, Double]) =
      SomeGraph.testLabelUndigraph.allLeastPathsAndBetweenness()

    assertEquals(allShortestPathsAndBetweenesses._1, expectedFirstSteps)
    assertEquals(allShortestPathsAndBetweenesses._2, expectedBetweennesses)
  }
}
