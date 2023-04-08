package net.walend.disentangle.examples

import net.walend.disentangle.graph.semiring.par.ParDijkstra
import net.walend.disentangle.graph.{AdjacencyLabelDigraph, IndexedLabelDigraph}
import net.walend.disentangle.graph.semiring.{AllPathsFirstSteps, Dijkstra, FewestNodes, FirstStepsTrait}

import scala.collection.parallel.immutable.ParSeq

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
  lazy val simpleShortPathLabels: Seq[(String, String, Option[FirstStepsTrait[String, Int]])] = Dijkstra.allPairsShortestPaths(edges)

  /**
   * The simplest API call finds paths with the fewest nodes, and supplies possible first steps to follow those paths.
   * AllPathsFirstSteps has some helper methods to generate the shortest paths.
   *
   * AllPathsFirstSteps takes a type parameter for Node's type, so you'll need to create a new one for your use.
   */
  lazy val support: AllPathsFirstSteps[String, Int, Int] = Dijkstra.defaultSupport[String]


  /**
   * Generate all the shortest paths in the graph in parallel
   */
  lazy val simpleShortPathLabelsFromPar: ParSeq[(String, String, Option[FirstStepsTrait[String, Int]])] = ParDijkstra.parAllPairsShortestPaths(edges)

  /**
   * The helper methods in AllPathsFirstSteps need a directed graph.
   * Use AllPathsFirstSteps.semiring's annihilator - None - for noEdgeExistsValue.
   */
  lazy val labelDigraph: AdjacencyLabelDigraph[String, support.Label] = AdjacencyLabelDigraph(edges = simpleShortPathLabels,noEdgeExistsValue = support.semiring.O)

  /**
   * Get a subgraph that holds all the possible shortest paths
   */
  lazy val subgraph: Set[labelDigraph.InnerEdgeType] = support.subgraphEdges(labelDigraph,"E","D")

  /**
   * Or just get the shortest paths
   */
  lazy val paths: Seq[Seq[labelDigraph.InnerNodeType]] = support.allLeastPaths(labelDigraph,"E","D")

  /**
   * To get all shortest paths from a single source (or sink), first create the initial label digraph.
   * You'll want to reuse this graph for different sources and sinks.
   */
  lazy val initialLabelDigraph: IndexedLabelDigraph[String, support.Label] = Dijkstra.createLabelDigraph(edges,support,support.convertEdgeToLabel(FewestNodes.convertEdgeToLabel))

  /**
   * Use the initialLabelDigraph to create the labels for the shortest paths from the source.
   */
  lazy val shortPathLabelsFromA: Seq[(String, String, support.Label)] = Dijkstra.dijkstraSingleSource(initialLabelDigraph,support)(initialLabelDigraph.innerNode("A").getOrElse(throw new IllegalStateException("A is not in this graph. How?")))

}
