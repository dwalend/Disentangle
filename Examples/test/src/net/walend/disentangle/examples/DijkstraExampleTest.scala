package net.walend.disentangle.examples

import org.scalatest.{Matchers, FlatSpec}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class DijkstraExampleTest extends FlatSpec with Matchers {

  "The Dijkstra example" should "produce expected results" in {
    val edges = DijkstraExample.edges

    val simpleShortPathLabels = DijkstraExample.simpleShortPathLabels

    val simpleShortPathLabelsFromPar = DijkstraExample.simpleShortPathLabelsFromPar

    val labelDigraph = DijkstraExample.labelDigraph

    val subgraph = DijkstraExample.subgraph

    val paths = DijkstraExample.paths

    val shortPathLabelsFromA = DijkstraExample.shortPathLabelsFromA
  }
}
