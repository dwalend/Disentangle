package net.walend.disentangle.examples

import munit.FunSuite
import net.walend.disentangle.graph.IndexedLabelDigraph
import net.walend.disentangle.graph.mutable.MatrixLabelDigraph
import net.walend.disentangle.graph.semiring.{AllPathsFirstSteps, FirstStepsTrait, FloydWarshall}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class FloydWarshallExampleTest extends FunSuite {
  import net.walend.disentangle.graph.SomeGraph.*

  val support: AllPathsFirstSteps[String, Int, Int] = FloydWarshall.defaultSupport[String]

  test("The Floyd-Warshall example should produce expected results") {

    val expectedShortPathGraph = MatrixLabelDigraph(
      edges = Vector((A,A,Some(support.FirstSteps(0,Set()))),
                      (A,B,Some(support.FirstSteps(1,Set(B)))),
                      (A,C,Some(support.FirstSteps(2,Set(B)))),
                      (A,D,Some(support.FirstSteps(3,Set(B)))),
                      (A,E,Some(support.FirstSteps(4,Set(B)))),
                      (A,H,Some(support.FirstSteps(5,Set(B)))),
                      (A,F,Some(support.FirstSteps(5,Set(B)))),
                      (B,B,Some(support.FirstSteps(0,Set()))),
                      (B,C,Some(support.FirstSteps(1,Set(C)))),
                      (B,D,Some(support.FirstSteps(2,Set(C)))),
                      (B,E,Some(support.FirstSteps(3,Set(C)))),
                      (B,H,Some(support.FirstSteps(4,Set(C)))),
                      (B,F,Some(support.FirstSteps(4,Set(C)))),
                      (C,B,Some(support.FirstSteps(3,Set(D)))),
                      (C,C,Some(support.FirstSteps(0,Set()))),
                      (C,D,Some(support.FirstSteps(1,Set(D)))),
                      (C,E,Some(support.FirstSteps(2,Set(D)))),
                      (C,H,Some(support.FirstSteps(3,Set(D)))),
                      (C,F,Some(support.FirstSteps(3,Set(D)))),
                      (D,B,Some(support.FirstSteps(2,Set(E)))),
                      (D,C,Some(support.FirstSteps(3,Set(E)))),
                      (D,D,Some(support.FirstSteps(0,Set()))),
                      (D,E,Some(support.FirstSteps(1,Set(E)))),
                      (D,H,Some(support.FirstSteps(2,Set(E)))),
                      (D,F,Some(support.FirstSteps(2,Set(E)))),
                      (E,B,Some(support.FirstSteps(1,Set(B)))),
                      (E,C,Some(support.FirstSteps(2,Set(B, H)))),
                      (E,D,Some(support.FirstSteps(3,Set(B, H)))),
                      (E,E,Some(support.FirstSteps(0,Set()))),
                      (E,H,Some(support.FirstSteps(1,Set(H)))),
                      (E,F,Some(support.FirstSteps(1,Set(F)))),
                      (H,B,Some(support.FirstSteps(4,Set(C)))),
                      (H,C,Some(support.FirstSteps(1,Set(C)))),
                      (H,D,Some(support.FirstSteps(2,Set(C)))),
                      (H,E,Some(support.FirstSteps(3,Set(C)))),
                      (H,H,Some(support.FirstSteps(0,Set()))),
                      (H,F,Some(support.FirstSteps(4,Set(C)))),
                      (F,F,Some(support.FirstSteps(0,Set())))
      ),
      nodes = Seq(A, B, C, D, E, H, F),
      noEdgeExistsValue = None
    )


    val shortPathGraph: IndexedLabelDigraph[String, Option[FirstStepsTrait[String, Int]]] = FloydWarshallExample.simpleShortPathGraph

    assert(shortPathGraph == expectedShortPathGraph)
    
    val expectedSubgraphEdges: Set[shortPathGraph.InnerEdgeType] = Set(
      shortPathGraph.edge(H,C),
      shortPathGraph.edge(E,B),
      shortPathGraph.edge(C,D),
      shortPathGraph.edge(E,H),
      shortPathGraph.edge(B,C)
    ).filter(_.isDefined).map(_.get)

    val subgraphEdges = FloydWarshallExample.subgraph

    assert(subgraphEdges == expectedSubgraphEdges)

    val expectedPaths = Vector(
      List(shortPathGraph.innerNode(E).get, shortPathGraph.innerNode(B).get, shortPathGraph.innerNode(C).get, shortPathGraph.innerNode(D).get),
      List(shortPathGraph.innerNode(E).get, shortPathGraph.innerNode(H).get, shortPathGraph.innerNode(C).get, shortPathGraph.innerNode(D).get)
    )
    
    val paths = FloydWarshallExample.paths

    assert(paths == expectedPaths)
  }
}
