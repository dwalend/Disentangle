package net.walend.disentangle.graph.cluster

import munit.{FunSuite, IgnoreSuite}
import net.walend.disentangle.graph.{AdjacencyUndigraph, NodePair, SomeGraph}
import net.walend.disentangle.graph.cluster.Agglomerate.{Initial as I, *}

@IgnoreSuite
class AgglomerateTest extends FunSuite {
  import SomeGraph._

  test("Testing with SomeGraph should not crash".ignore) {

    val testGraph = SomeGraph.testUndigraph //todo work with the karate school graph
    val initialCluster = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialCluster)
  }

  test("An empty graph should result in a List with an empty graph".ignore) {
    val testGraph = AdjacencyUndigraph()

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    assertEquals(clusters,List(AdjacencyUndigraph()))
  }


  test("A graph with one node should not crash".ignore) {
    val testGraph = AdjacencyUndigraph(nodes = List(A))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    assertEquals(clusters,List(AdjacencyUndigraph(nodes = List(Agglomerate.Initial(A)))))
  }

  test("A graph with two isolated nodes should not crash".ignore) {
    val testGraph = AdjacencyUndigraph(nodes = List(A,B))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedClusters: List[ClusterGraph] = List(AdjacencyUndigraph(nodes = Seq(I(A), I(B))), AdjacencyUndigraph(nodes = Seq(Isolates(Set(I(A), I(B)),2))))

    assertEquals(clusters,expectedClusters)
  }

  test("A graph with two linked nodes should form a cycle".ignore) {
    val testGraph = AdjacencyUndigraph(edges = Seq(NodePair(A,B)))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedInitial: AdjacencyUndigraph[Cluster] = AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B))))
    val expectedCycle = Cycle(AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)))),Seq(I(B),I(A)),2)
    val expectedClusters: List[ClusterGraph] = List(expectedInitial, AdjacencyUndigraph(nodes = Seq(expectedCycle)))

    assertEquals(clusters,expectedClusters)
  }

  test("A graph with three linked nodes should form a cycle".ignore)  {
    val testGraph = AdjacencyUndigraph(edges = Seq(NodePair(A,B),NodePair(B,C),NodePair(C,A)),nodes = Seq(A,B,C))

    val initialClusters = Agglomerate.initialClusterFromGraph(testGraph)
    val clusters: List[ClusterGraph] = Agglomerate.agglomerate(initialClusters)

    val expectedInitial: AdjacencyUndigraph[Cluster] = AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)),NodePair(I(B),I(C)),NodePair(I(C),I(A))))
    val expectedCycle = Cycle(AdjacencyUndigraph(edges = Seq(NodePair(I(A),I(B)),NodePair(I(B),I(C)),NodePair(I(C),I(A)))),Seq(I(C),I(B),I(A)),2)
    val expectedClusters: List[ClusterGraph] = List(expectedInitial, AdjacencyUndigraph(nodes = Seq(expectedCycle)))

    assertEquals(clusters,expectedClusters)
  }


  test("A star graph should form something reasonable".ignore) {

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
