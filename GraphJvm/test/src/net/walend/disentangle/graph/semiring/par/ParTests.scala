package net.walend.disentangle.graph.semiring.par

import munit.FunSuite
import net.walend.disentangle.graph.SomeGraph
import net.walend.disentangle.graph.semiring.{AllPathsFirstStepsTest, BrandesTest, FewestNodes, FewestNodesTest, UndirectedGraphTest}


class ParTests extends FunSuite{

  test("Dijkstra's algorithm should produce the correct label graph for Somegraph using default values for the parallel algorithm and AllPathsFirstSteps") {
    val answers: AllPathsFirstStepsTest = new AllPathsFirstStepsTest()

    val arcs = ParDijkstra.parAllPairsShortestPaths(SomeGraph.testDigraph.edges, Seq.from(SomeGraph.testDigraph.nodes))

    assertEquals(arcs.size, answers.expectedArcs.size)
    assertEquals(Set.from(arcs), answers.expectedArcs)
  }

  test("Dijkstra's algorithm should produce the correct label graph for Somegraph using default values for the parallel algorithm with the implicit extension method and AllPathsFirstSteps") {
    val answers = new AllPathsFirstStepsTest()
    val arcs = SomeGraph.testDigraph.parAllPairsShortestPaths

    assertEquals(arcs.size, answers.expectedArcs.size)
    assertEquals(Set.from(arcs), answers.expectedArcs)
  }

  test("Parallel Dijkstra's algorithm should produce the correct label graph for Somegraph and FewestNodes") {
    val answers = new FewestNodesTest()
    val labels = ParDijkstra.parAllPairsLeastPaths(SomeGraph.testDigraph.edges, FewestNodes, FewestNodes.convertEdgeToLabel, Seq.from(SomeGraph.testDigraph.nodes))

    assertEquals(labels.size, answers.expectedArcs.size)
    assertEquals(Set.from(labels), answers.expectedArcs)
  }

  test("Parallel Dijkstra's algorithm should produce the correct label graph for Somegraph using the implicit method on testDigraph and FewestNodes") {
    val answers = new FewestNodesTest()

    val labels = SomeGraph.testDigraph.parAllPairsLeastPaths(FewestNodes, FewestNodes.convertEdgeToLabel)

    assertEquals(labels.size, answers.expectedArcs.size)
    assertEquals(Set.from(labels), answers.expectedArcs)
  }

  test("Dijkstra's algorithm should produce the correct label graph for Somegraph in parallel on an UndirectedGraph") {
    val answers: UndirectedGraphTest = new UndirectedGraphTest()
    val allShortestPaths = SomeGraph.testLabelUndigraph.parAllPairsShortestPaths

    assertEquals(Vector.from(allShortestPaths), answers.expectedShortestPaths)
  }
  
  test("Brandes' algorithm should produce the correct label graph and betweenness using the implicit method on a Digraph in parallel") {
    val answers = new BrandesTest()

    val labelGraphAndBetweenness = ParBrandes.parAllLeastPathsAndBetweenness(SomeGraph.testDigraph.edges, Seq.from(SomeGraph.testDigraph.nodes), answers.support, FewestNodes.convertEdgeToLabel)
    answers.checkBrandesResults((Seq.from(labelGraphAndBetweenness._1), Map.from(labelGraphAndBetweenness._2)))
  }


}
