package net.walend.disentangle.examples

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraLeastWeightsExampleTest extends AnyFlatSpec with Matchers {

  "The Dijkstra with least weights example" should "produce expected results" in {

    val shortPathLabels = DijkstraLeastWeightsExample.leastPathLabels

    val shortPathLabelsFromPar = DijkstraLeastWeightsExample.leastPathLabelsFromPar

    val labelDigraph = DijkstraLeastWeightsExample.labelDigraph

    val subgraph = DijkstraLeastWeightsExample.subgraph

    val paths = DijkstraLeastWeightsExample.paths

    val shortPathLabelsFromA = DijkstraLeastWeightsExample.shortPathLabelsFromA
  }
}
