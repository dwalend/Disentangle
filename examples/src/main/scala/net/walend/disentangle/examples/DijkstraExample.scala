package net.walend.disentangle.examples

import net.walend.disentangle.graph.AdjacencyLabelDigraph
import net.walend.disentangle.graph.semiring.{AllPathsFirstSteps, FirstStepsTrait, Dijkstra}

/**
 * Use Dijkstra's algorithms to find either single-source or all-pairs shortest paths using the default semiring.
 *
 * @author dwalend
 * @since v0.2.0
 */
object DijkstraExample {
  
  /**
   * Edges are just a Seq of Tuple3[Node,Node,Edge]
   */
  val edges: Seq[(String, String, String)] = Seq(
                                                  ("A","B","ab"),
                                                  ("B","C","bc"),
                                                  ("C","D","cd"),
                                                  ("D","E","de"),
                                                  ("E","F","ef"),
                                                  ("E","B","eb"),
                                                  ("E","H","eh"),
                                                  ("H","C","hc")
                                                )

  //todo single source and single sink

  /**
   * Generate all the shortest paths in the graph
   */
  val simpleShortPathLabels: Seq[(String, String, Option[FirstStepsTrait[String, Int]])] = Dijkstra.allPairsLeastPaths(edges)

  /**
   * The simplest API call finds paths with the fewest nodes, and supplies possible first steps to follow those paths.
   * AllPathsFirstSteps has some helper methods to generate the shortest paths.
   *
   * AllPathsFirstSteps takes a type parameter for Node's type, so you'll need to create a new one for your use.
   */
  val support: AllPathsFirstSteps[String, Int, Int] = Dijkstra.defaultSupport[String]

  /**
   * The helper methods in AllPathsFirstSteps need a directed graph.
   * Use AllPathsFirstSteps.semiring's annihilator - None - for noEdgeExistsValue.
   */
  val labelDigraph: AdjacencyLabelDigraph[String, support.Label] = AdjacencyLabelDigraph(edges = simpleShortPathLabels,noEdgeExistsValue = support.semiring.O)

  /**
   * Get a subgraph that holds all the possible shortest paths
   */
  val subgraph: Set[labelDigraph.InnerEdgeType] = support.subgraphEdges(labelDigraph,"E","D")

  /**
   * Or just get the shortest paths
   */
  val paths: Seq[Seq[labelDigraph.InnerNodeType]] = support.allLeastPaths(labelDigraph,"E","D")


  
}
