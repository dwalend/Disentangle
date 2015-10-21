package net.walend.disentangle.examples

import org.scalatest.{Matchers, FlatSpec}

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class BrandesExampleTest extends FlatSpec with Matchers {

  "The Brandes example" should "produce expected results" in {

    BrandesExample.shortestPathsAndBetweenness

    BrandesExample.shortestPathsAndBetweennessFromPar

    BrandesExample.subgraph

    BrandesExample.paths
  }
}
