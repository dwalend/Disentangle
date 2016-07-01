package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.SomeGraph
import net.walend.disentangle.graph.cluster.Agglomerate.ClusterGraph
import org.scalatest.{FlatSpec, Matchers}


class AgglomerateTest extends FlatSpec with Matchers {

  "Testing with SomeGraph" should "not crash" in {

    val testGraph = SomeGraph.testUndigraph //todo work with the karate school graph
    val initialCluster = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialCluster)
  }
}
