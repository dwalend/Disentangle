package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.SomeGraph
import net.walend.disentangle.graph.cluster.Agglomerate.ClusterGraph


object AgglomerateTest {

  val testGraph = SomeGraph.testUndigraph//todo work with the karate school graph
  val initialCluster = Agglomerate.initialClusterFromGraph(testGraph)
  val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialCluster)
}
