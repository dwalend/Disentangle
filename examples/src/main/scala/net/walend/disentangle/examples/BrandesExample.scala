package net.walend.disentangle.examples

import scala.collection.parallel
import scala.collection.parallel.immutable.{ParMap, ParSeq}

import net.walend.disentangle.graph.AdjacencyLabelDigraph
import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps
import net.walend.disentangle.graph.semiring.{FirstStepsTrait, Dijkstra, AllPathsFirstSteps, Brandes}

/**
 * Use Brandes' algorithms to find least paths and betweenness for a directed graph.
 *
 * @author dwalend
 * @since v0.2.0
 */
object BrandesExample {

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
   * The labels from Brandes use node indexes from a directed graph, so it's best to control those via the optional nodeOrder parameter
   */
  lazy val nodeOrder = Array("A","B","C","D","E","F","H")

  /**
   * Find shortest paths and betweenness for the graph
   */
  lazy val shortestPathsAndBetweenness: (IndexedSeq[(String, String, Option[BrandesSteps[String, Int]])], Map[String, Double]) = Brandes.allLeastPathsAndBetweenness(edges,nodeOrder)

  /**
   * Find shortest paths and betweenness for the graph in parallel
   */
  lazy val shortestPathsAndBetweennessFromPar: (parallel.ParSeq[(String, String, Option[BrandesSteps[String, Int]])], ParMap[String, Double]) = Brandes.parAllLeastPathsAndBetweenness(edges,nodeOrder)

  /**
   * The first item in the tuple holds edges labels.
   */
  lazy val shortPathLabels: IndexedSeq[(String, String, Option[BrandesSteps[String, Int]])] = shortestPathsAndBetweenness._1

  /**
   * The second item in the tuple holds the betweenness for the graph.
   */
  lazy val betweennesses: Map[String, Double] = shortestPathsAndBetweenness._2

  /**
   * BrandesSupport has some helper methods to generate the shortest paths.
   */
  lazy val support: Brandes.BrandesSupport[String, Int, Int] = Brandes.BrandesSupport[String]()

  /**
   * The helper methods in AllPathsFirstSteps need a directed graph.
   * Use AllPathsFirstSteps.semiring's annihilator - None - for noEdgeExistsValue.
   */
  lazy val labelDigraph: AdjacencyLabelDigraph[String, support.Label] = AdjacencyLabelDigraph(edges = shortPathLabels,
                                                                                              nodes = nodeOrder,
                                                                                              noEdgeExistsValue = support.semiring.O)

   /**
    * Get a subgraph that holds all the possible shortest paths
    */
   lazy val subgraph: Set[labelDigraph.InnerEdgeType] = support.subgraphEdges(labelDigraph,"E","D")

   /**
    * Or just get the shortest paths
    */
   lazy val paths: Seq[Seq[labelDigraph.InnerNodeType]] = support.allLeastPaths(labelDigraph,"E","D")
}
