package net.walend.disentangle.examples

import net.walend.disentangle.graph.AdjacencyLabelDigraph
import net.walend.disentangle.graph.semiring.{AllPathsFirstSteps, FirstStepsTrait, Dijkstra}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
object DijkstraExamples {

  //todo other type for edge? Maybe Int or Unit or Boolean

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

  /**
   * Generate all the shortest paths in the graph
   */
  val simpleShortPathLabels: Seq[(String, String, Option[FirstStepsTrait[String, Int]])] = Dijkstra.allPairsShortestPaths(edges)

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

  //todo use a semiring - LeastWeights of a hash of the edges? MostProbable? to show the full API




}
