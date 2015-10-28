package net.walend.disentangle.examples

import scala.collection.parallel.immutable.ParSeq

import net.walend.disentangle.graph.{IndexedLabelDigraph, AdjacencyLabelDigraph}
import net.walend.disentangle.graph.semiring.{LeastWeights, AllPathsFirstSteps, Dijkstra}

/**
 * Use Dijkstra's algorithms to find either single-source or all-pairs shortest paths using a custom semiring, LeastWeights.
 *
 * @author dwalend
 * @since v0.2.0
 */
object DijkstraLeastWeightsExample {

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
   * A semiring support instance that uses double-valued labels to find the shortest paths.
   */
  lazy val support: AllPathsFirstSteps[String, Double, Double] = new AllPathsFirstSteps(LeastWeights)

  /**
   * This time, we'll need to supply a function that can convert from a String to a Double to build up the initial graph
   * of edges. You'll probably have something more significant than this hack.
   */
  def stringToDouble(fromNode:String,toNode:String,edge:String):Double = edge.map(_.hashCode().toDouble).product

  /**
   * Build on AllPathsFirstSteps' convert method
   */
  lazy val labelForEdge: (String, String, String) => support.Label = support.convertEdgeToLabel[String](stringToDouble)

  /**
   * Generate the first steps for all paths in the graph
   */
  lazy val leasttPathLabels: Seq[(String, String, support.Label)] = Dijkstra.allPairsLeastPaths(edges,support,labelForEdge)

  /**
   * Generate the first steps for all paths in the graph in parallel
   */
  lazy val leastPathLabelsFromPar: ParSeq[(String, String, support.Label)] = Dijkstra.parAllPairsLeastPaths(edges,support,labelForEdge)

  /**
   * The helper methods in AllPathsFirstSteps need a directed graph.
   * Use AllPathsFirstSteps.semiring's annihilator - None - for noEdgeExistsValue.
   */
  lazy val labelDigraph: AdjacencyLabelDigraph[String, support.Label] = AdjacencyLabelDigraph(edges = leasttPathLabels,noEdgeExistsValue = support.semiring.O)

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
  lazy val initialLabelDigraph: IndexedLabelDigraph[String, support.Label] = Dijkstra.createLabelDigraph(edges,support,labelForEdge)

  /**
   * Use the initialLabelDigraph to create the labels for the shortest paths from the source
   */
  lazy val shortPathLabelsFromA: Seq[(String, String, support.Label)] = Dijkstra.dijkstraSingleSource(initialLabelDigraph,support)(initialLabelDigraph.innerNode("A").getOrElse(throw new IllegalStateException("A is not in this graph. How?")))

}
