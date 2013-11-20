package walend.scalax.semiring

/**
 *
 *
 * @author dwalend
 * @since v1
 */
import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

import TransitiveClosureSemiring.ImplicitLabel._

class AllShortestPathsPredecessorsTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (B~+#>B)(Some(PreviousStep(0,Set()))),
    (B~+#>C)(Some(PreviousStep(1,Set(B)))),
    (B~+#>D)(Some(PreviousStep(2,Set(C)))),   //C
    (B~+#>E)(Some(PreviousStep(3,Set(D)))),   //C D
    (B~+#>H)(Some(PreviousStep(4,Set(E)))),   //C D E
    (B~+#>F)(Some(PreviousStep(4,Set(E)))),   //C D E
    (F~+#>F)(Some(PreviousStep(0,Set()))),
    (C~+#>C)(Some(PreviousStep(0,Set()))),
    (C~+#>D)(Some(PreviousStep(1,Set(C)))),
    (C~+#>E)(Some(PreviousStep(2,Set(D)))),   //  D
    (C~+#>B)(Some(PreviousStep(3,Set(E)))),   //  D E
    (C~+#>H)(Some(PreviousStep(3,Set(E)))),   //  D E
    (C~+#>F)(Some(PreviousStep(3,Set(E)))),   //  D E
    (G~+#>G)(Some(PreviousStep(0,Set()))),
    (D~+#>D)(Some(PreviousStep(0,Set()))),
    (D~+#>E)(Some(PreviousStep(1,Set(D)))),
    (D~+#>B)(Some(PreviousStep(2,Set(E)))),   //    E
    (D~+#>H)(Some(PreviousStep(2,Set(E)))),   //    E
    (D~+#>F)(Some(PreviousStep(2,Set(E)))),   //    E
    (D~+#>C)(Some(PreviousStep(3,Set(B, H)))),
    (H~+#>H)(Some(PreviousStep(0,Set()))),
    (H~+#>C)(Some(PreviousStep(1,Set(H)))),
    (H~+#>D)(Some(PreviousStep(2,Set(C)))),
    (H~+#>E)(Some(PreviousStep(3,Set(D)))),
    (H~+#>B)(Some(PreviousStep(4,Set(E)))),
    (H~+#>F)(Some(PreviousStep(4,Set(E)))),
    (E~+#>E)(Some(PreviousStep(0,Set()))),
    (E~+#>B)(Some(PreviousStep(1,Set(E)))),
    (E~+#>H)(Some(PreviousStep(1,Set(E)))),
    (E~+#>C)(Some(PreviousStep(2,Set(B, H)))),
    (E~+#>F)(Some(PreviousStep(1,Set(E)))),
    (E~+#>D)(Some(PreviousStep(3,Set(C)))),
    (A~+#>A)(Some(PreviousStep(0,Set()))),
    (A~+#>B)(Some(PreviousStep(1,Set(A)))),
    (A~+#>C)(Some(PreviousStep(2,Set(B)))),
    (A~+#>D)(Some(PreviousStep(3,Set(C)))),
    (A~+#>E)(Some(PreviousStep(4,Set(D)))),
    (A~+#>H)(Some(PreviousStep(5,Set(E)))),
    (A~+#>F)(Some(PreviousStep(5,Set(E)))))

  //res1: Map[String,Double] = Map(E -> 9.0, F -> 0.0, A -> 0.0, G -> 0.0, B -> 7.0, C -> 6.0, H -> 2.0, D -> 7.0)
  /*
  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsPredecessorsGraphBuilder[String])

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

    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsPredecessorsGraphBuilder[String])

    (labelGraph.edges.toEdgeInSet.to[Set] -- expectedEdges) should be (Set.empty)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPaths,new AllShortestPathsPredecessorsGraphBuilder[String])

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }
                 */
  "Brandes' algorithm" should "produce the correct betweenness for SomeGraph" in {

//    val graph = SomeGraph.testGraph

    val nodes = Set(A,B,C,D)
    val edges = Set(ab,bc,cd)
    val graph = Graph.from(nodes,edges)

    val expectedBetweenness = Map(A -> 0.0, B -> 2.0, C -> 2.0, D -> 0.0)

    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraphAndBetweenness = Brandes.shortestPathsAndBetweenness(graph)(allShortestPaths,new AllShortestPathsPredecessorsGraphBuilder[String])

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }


}
