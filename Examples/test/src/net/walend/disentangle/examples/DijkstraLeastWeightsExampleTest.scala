package net.walend.disentangle.examples

import munit.FunSuite

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraLeastWeightsExampleTest extends FunSuite {

  test("The Dijkstra with least weights example should produce expected results"){

    val shortPathLabels = DijkstraLeastWeightsExample.leastPathLabels

//todo    val shortPathLabelsFromPar = DijkstraLeastWeightsExample.leastPathLabelsFromPar

    val labelDigraph = DijkstraLeastWeightsExample.labelDigraph

    val subgraph = DijkstraLeastWeightsExample.subgraph

    val paths = DijkstraLeastWeightsExample.paths

    val shortPathLabelsFromA = DijkstraLeastWeightsExample.shortPathLabelsFromA
  }
}
