package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._
import net.walend.digraph.AdjacencyLabelDigraph

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class AllPathsFirstStepsTest extends FlatSpec with Matchers {

  val expectedArcs = Set[(String,String,Option[FirstSteps[String,Int]])](
    (A,A,Some(FirstSteps(0,Set()))),
    (A,B,Some(FirstSteps(1,Set(B)))),
    (A,C,Some(FirstSteps(2,Set(B)))),
    (A,D,Some(FirstSteps(3,Set(B)))),
    (A,E,Some(FirstSteps(4,Set(B)))),
    (A,F,Some(FirstSteps(5,Set(B)))),
    (A,H,Some(FirstSteps(5,Set(B)))),
    (B,B,Some(FirstSteps(0,Set()))),
    (B,C,Some(FirstSteps(1,Set(C)))),
    (B,D,Some(FirstSteps(2,Set(C)))),
    (B,E,Some(FirstSteps(3,Set(C)))),
    (B,F,Some(FirstSteps(4,Set(C)))),
    (B,H,Some(FirstSteps(4,Set(C)))),
    (C,B,Some(FirstSteps(3,Set(D)))),
    (C,C,Some(FirstSteps(0,Set()))),
    (C,D,Some(FirstSteps(1,Set(D)))),
    (C,E,Some(FirstSteps(2,Set(D)))),
    (C,F,Some(FirstSteps(3,Set(D)))),
    (C,H,Some(FirstSteps(3,Set(D)))),
    (D,B,Some(FirstSteps(2,Set(E)))),
    (D,C,Some(FirstSteps(3,Set(E)))),
    (D,D,Some(FirstSteps(0,Set()))),
    (D,E,Some(FirstSteps(1,Set(E)))),
    (D,F,Some(FirstSteps(2,Set(E)))),
    (D,H,Some(FirstSteps(2,Set(E)))),
    (E,B,Some(FirstSteps(1,Set(B)))),
    (E,C,Some(FirstSteps(2,Set(B, H)))),
    (E,D,Some(FirstSteps(3,Set(B, H)))),
    (E,E,Some(FirstSteps(0,Set()))),
    (E,F,Some(FirstSteps(1,Set(F)))),
    (E,H,Some(FirstSteps(1,Set(H)))),
    (F,F,Some(FirstSteps(0,Set()))),
    (G,G,Some(FirstSteps(0,Set()))),
    (H,B,Some(FirstSteps(4,Set(C)))),
    (H,C,Some(FirstSteps(1,Set(C)))),
    (H,D,Some(FirstSteps(2,Set(C)))),
    (H,E,Some(FirstSteps(3,Set(C)))),
    (H,F,Some(FirstSteps(4,Set(C)))),
    (H,H,Some(FirstSteps(0,Set())))
  )

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.arcs,testGraph.nodes,support,support.convertArcToLabelFunc[String](FewestNodes.convertArcToLabel))

    labelGraph.arcs.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val arcs = Dijkstra.allPairsShortestPaths(testGraph.arcs,testGraph.nodes,support,support.convertArcToLabelFunc[String](FewestNodes.convertArcToLabel))

    arcs.size should be (expectedArcs.size)
    arcs.to[Set] should be (expectedArcs)

    val resultDigraph = AdjacencyLabelDigraph(arcSeq = arcs,noArcExistsValue = support.semiring.O)

    //todo add a test
    /*
    for(arc <- arcs) {
      println(s"${arc._1},${arc._2}  ${support.subgraphEdges(resultDigraph,arc._1,arc._2)}")
    }
    */
  }
}
