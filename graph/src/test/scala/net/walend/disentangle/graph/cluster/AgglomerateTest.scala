package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{AdjacencyUndigraph, NodePair, SomeGraph}
import net.walend.disentangle.graph.cluster.Agglomerate.{Initial => I, _}
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

  "A graph with two isolated nodes " should "not crash" in {
    val testGraph = AdjacencyUndigraph(nodes = List(A,B))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedClusters: List[ClusterGraph] = List(AdjacencyUndigraph(nodes = Seq(I(A), I(B))), AdjacencyUndigraph(nodes = Seq(Isolates(Set(I(A), I(B)),2))))

    clusters should be(expectedClusters)
  }

  "A graph with two linked nodes " should "form a cycle" in {
    val testGraph = AdjacencyUndigraph(edges = Seq(NodePair(A,B)))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedInitial: AdjacencyUndigraph[Cluster] = AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B))))
    val expectedCycle = Cycle(AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)))),Seq(I(A),I(B)),2)
    val expectedClusters: List[ClusterGraph] = List(expectedInitial, AdjacencyUndigraph(nodes = Seq(expectedCycle)))

    clusters should be(expectedClusters)
  }

  "A graph with three linked nodes " should "form a cycle" in  {
    val testGraph = AdjacencyUndigraph(edges = Seq(NodePair(A,B),NodePair(B,C),NodePair(C,A)),nodes = Seq(A,B,C))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedInitial: AdjacencyUndigraph[Cluster] = AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)),NodePair(I(B),I(C)),NodePair(I(C),I(A))))
    val expectedCycle = Cycle(AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)),NodePair(I(B),I(C)),NodePair(I(C),I(A)))),Seq(I(B),I(A),I(C)),2)
    val expectedClusters: List[ClusterGraph] = List(expectedInitial, AdjacencyUndigraph(nodes = Seq(expectedCycle)))

    clusters should be(expectedClusters)
  }


  "A star graph" should "form something reasonable" in {

    val edges = Seq((A,B),(B,C))

    val testGraph = AdjacencyUndigraph.fromPairs(edges = edges,nodes = Seq(A,B,C))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedSiblings = Sibling(AdjacencyUndigraph(nodes = Seq(I(B), I(C))),I(A),2)
    val expectedCaterpillar = Caterpillar(AdjacencyUndigraph(nodes = Seq(I(A))),List(I(A)),expectedSiblings,2)

    val expectedCycle = Cycle(
      AdjacencyUndigraph(edges = Seq(NodePair(expectedCaterpillar,expectedSiblings))),
        Seq(expectedCaterpillar,expectedSiblings),
        3)

    val expectedClusters: List[ClusterGraph] = List(
      AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)), NodePair(I(A),I(C)))),
      AdjacencyUndigraph(edges = Seq(NodePair(expectedCaterpillar,expectedSiblings))),
      AdjacencyUndigraph(nodes = Seq(expectedCycle))
    )

    println(clusters)

//    clusters should be(expectedClusters)

  }


}
