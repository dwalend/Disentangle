package net.walend.disentangle.examples

import munit.FunSuite

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraExampleTest extends FunSuite {

  test("The Dijkstra example should produce expected results") {
    val edges = DijkstraExample.edges

    val simpleShortPathLabels = DijkstraExample.simpleShortPathLabels

//todo    val simpleShortPathLabelsFromPar = DijkstraExample.simpleShortPathLabelsFromPar

    val labelDigraph = DijkstraExample.labelDigraph

    val subgraph = DijkstraExample.subgraph

    val paths = DijkstraExample.paths

    val shortPathLabelsFromA = DijkstraExample.shortPathLabelsFromA
  }
}
