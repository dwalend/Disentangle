package net.walend.disentangle.examples

import org.scalatest.{Matchers, FlatSpec}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraLeastWeightsExampleTest extends FlatSpec with Matchers {

  "The Dijkstra with least weights example" should "produce expected results" in {

    val shortPathLabels = DijkstraLeastWeightsExample.leastPathLabels

    val shortPathLabelsFromPar = DijkstraLeastWeightsExample.leastPathLabelsFromPar

    val labelDigraph = DijkstraLeastWeightsExample.labelDigraph

    val subgraph = DijkstraLeastWeightsExample.subgraph

    val paths = DijkstraLeastWeightsExample.paths

    val shortPathLabelsFromA = DijkstraLeastWeightsExample.shortPathLabelsFromA
  }
}
