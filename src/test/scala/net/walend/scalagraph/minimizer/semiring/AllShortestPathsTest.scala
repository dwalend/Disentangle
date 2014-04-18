package net.walend.scalagraph.minimizer.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.Graph
import MLDiEdge._

/**
 *
 *
 * @author dwalend
 * @since v1
 */
class AllShortestPathsTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (B~+>B)(Some(Steps(0,Set()))),
    (B~+>F)(Some(Steps(4,Set(C)))),
    (B~+>C)(Some(Steps(1,Set(C)))),
    (B~+>D)(Some(Steps(2,Set(C)))),
    (B~+>H)(Some(Steps(4,Set(C)))),
    (B~+>E)(Some(Steps(3,Set(C)))),
    (F~+>F)(Some(Steps(0,Set()))),
    (C~+>B)(Some(Steps(3,Set(D)))),
    (C~+>F)(Some(Steps(3,Set(D)))),
    (C~+>C)(Some(Steps(0,Set()))),
    (C~+>D)(Some(Steps(1,Set(D)))),
    (C~+>H)(Some(Steps(3,Set(D)))),
    (C~+>E)(Some(Steps(2,Set(D)))),
    (G~+>G)(Some(Steps(0,Set()))),
    (D~+>B)(Some(Steps(2,Set(E)))),
    (D~+>F)(Some(Steps(2,Set(E)))),
    (D~+>C)(Some(Steps(3,Set(E)))),
    (D~+>D)(Some(Steps(0,Set()))),
    (D~+>H)(Some(Steps(2,Set(E)))),
    (D~+>E)(Some(Steps(1,Set(E)))),
    (H~+>B)(Some(Steps(4,Set(C)))),
    (H~+>F)(Some(Steps(4,Set(C)))),
    (H~+>C)(Some(Steps(1,Set(C)))),
    (H~+>D)(Some(Steps(2,Set(C)))),
    (H~+>H)(Some(Steps(0,Set()))),
    (H~+>E)(Some(Steps(3,Set(C)))),
    (E~+>B)(Some(Steps(1,Set(B)))),
    (E~+>F)(Some(Steps(1,Set(F)))),
    (E~+>C)(Some(Steps(2,Set(B, H)))),
    (E~+>D)(Some(Steps(3,Set(B, H)))),
    (E~+>H)(Some(Steps(1,Set(H)))),
    (E~+>E)(Some(Steps(0,Set()))),
    (A~+>B)(Some(Steps(1,Set(B)))),
    (A~+>F)(Some(Steps(5,Set(B)))),
    (A~+>C)(Some(Steps(2,Set(B)))),
    (A~+>D)(Some(Steps(3,Set(B)))),
    (A~+>H)(Some(Steps(5,Set(B)))),
    (A~+>E)(Some(Steps(4,Set(B)))),
    (A~+>A)(Some(Steps(0,Set()))))

  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(allShortestPaths.semiring,new AllShortestPathsGraphBuilder[String](allShortestPaths.semiring))(graph)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==allShortestPaths.semiring.I,"The edge label for all self-edges should be "+allShortestPaths.semiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(allShortestPaths.semiring,new AllShortestPathsGraphBuilder[String](allShortestPaths.semiring))(graph)

    (labelGraph.edges.toOuter.to[Set] -- expectedEdges) should be (Set.empty)

    labelGraph.edges.toOuter should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPaths[String]

    val labelGraph = Dijkstra.allPairsShortestPaths(allShortestPaths,new AllShortestPathsGraphBuilder[String](allShortestPaths.semiring))(graph)

    labelGraph.edges.toOuter should be (expectedEdges)
  }

}
