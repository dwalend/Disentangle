package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{AdjacencyUndigraph, SomeGraph}
import net.walend.disentangle.graph.cluster.Agglomerate.ClusterGraph
import org.scalatest.{FlatSpec, Matchers}

import SomeGraph._


class AgglomerateTest extends FlatSpec with Matchers {

  "Testing with SomeGraph" should "not crash" in {

    val testGraph = SomeGraph.testUndigraph //todo work with the karate school graph
    val initialCluster = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialCluster)
  }

  "An empty graph " should "result in a List with an empty graph" in {
    val testGraph = AdjacencyUndigraph()

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    clusters should be(List(AdjacencyUndigraph()))
  }


  "A graph with one node " should "not crash" in {
    val testGraph = AdjacencyUndigraph(nodes = List(A))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    clusters should be(List(AdjacencyUndigraph(nodes = List(Agglomerate.Initial(A)))))
  }
/*
  "A star graph" should "form a wheel" in {

    import SomeGraph._

    val edges = Seq(A->B,A->C)
  }
  */
}
