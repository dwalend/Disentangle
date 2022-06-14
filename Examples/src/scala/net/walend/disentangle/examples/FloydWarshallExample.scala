package net.walend.disentangle.examples

import net.walend.disentangle.graph.IndexedLabelDigraph
import net.walend.disentangle.graph.semiring.{FloydWarshall, AllPathsFirstSteps, FirstStepsTrait}

/**
 * Use Dijkstra's algorithms to find all-pairs shortest paths using the default semiring.
 *
 * @author dwalend
 * @since v0.2.0
 */
object FloydWarshallExample {

  /**
   * Edges are just a Seq of Tuple3[Node,Node,Edge]
   */
  lazy val edges: Seq[(String, String, String)] = Seq(
    ("A","B","ab"),
    ("B","C","bc"),
    ("C","D","cd"),
    ("D","E","de"),
    ("E","F","ef"),
    ("E","B","eb"),
    ("E","H","eh"),
    ("H","C","hc")
  )

  /**
   * Generate all the shortest paths in the graph
   */
  lazy val simpleShortPathGraph: IndexedLabelDigraph[String, Option[FirstStepsTrait[String, Int]]] = FloydWarshall.allPairsShortestPaths(edges)

  /**
   * The simplest API call finds paths with the fewest nodes, and supplies possible first steps to follow those paths.
   * AllPathsFirstSteps has some helper methods to generate the shortest paths.
   *
   * AllPathsFirstSteps takes a type parameter for Node's type, so you'll need to create a new one for your use.
   */
  lazy val support: AllPathsFirstSteps[String, Int, Int] = FloydWarshall.defaultSupport[String]

  /**
   * Get a subgraph that holds all the possible shortest paths
   */
  lazy val subgraph: Set[simpleShortPathGraph.InnerEdgeType] = support.subgraphEdges(simpleShortPathGraph,"E","D")

  /**
   * Or just get the shortest paths
   */
  lazy val paths: Seq[Seq[simpleShortPathGraph.InnerNodeType]] = support.allLeastPaths(simpleShortPathGraph,"E","D")

}
