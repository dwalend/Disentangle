package net.walend.disentangle.examples

import scala.collection.parallel.immutable.ParSeq

import org.scalatest.{Matchers, FlatSpec}

import net.walend.disentangle.graph.{IndexedLabelDigraph, AdjacencyLabelDigraph}
import net.walend.disentangle.graph.semiring.{FewestNodes, AllPathsFirstSteps, Dijkstra, FirstStepsTrait}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraExampleTest extends FlatSpec with Matchers {

  val edges = DijkstraExample.edges

  val simpleShortPathLabels = DijkstraExample.simpleShortPathLabels

  val simpleShortPathLabelsFromPar: ParSeq[(String, String, Option[FirstStepsTrait[String, Int]])] = Dijkstra.parAllPairsShortestPaths(edges)

  val labelDigraph = DijkstraExample.labelDigraph

  val subgraph = DijkstraExample.subgraph

  val paths = DijkstraExample.paths

  val shortPathLabelsFromA = DijkstraExample.shortPathLabelsFromA

}
