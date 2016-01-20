package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{SomeGraph, AdjacencyLabelUndigraph, NodePair, LabelUndigraph, Undigraph}

import scala.collection.GenSet

/**
  * Creates clusters of nodes in a directed graph by grouping them via the Louvain algorithm.
  *
  * @see https://en.wikipedia.org/wiki/Louvain_Modularity
  * @author dwalend
  * @since v0.2.1
  */
object Louvain {

/*
  type Node = String
  type Weight = Int

  type Edge = (Cluster,Weight)

  type Cluster = Set[Node]
*/
//  type Graph = GenSet[(Cluster,Set[Edge])]


//  case class Graph(subgraphs:Set[Graph],edges:Set[(NodePair[Graph],Edge)])

  abstract class Cluster {
//    def edges:Map[Cluster,Int]
  }

  case class Leaf[Node](node:Node) extends Cluster

  case class Subgraph(graph:LabelUndigraph[Cluster,Int]) extends Cluster  //todo replace Int with something from a Semiring, maybe

  type ClusterGraph = LabelUndigraph[Cluster,Int]

//  type Edge = ClusterGraph.OuterEdgeType
//  case class Cluster(graph:LabelUndigraph[Cluster,Int],archNode:String)

//  val graph = LabelUndigraph()

  val testGraph = SomeGraph.testUndigraph//todo work with the karate school graph

  val initialCluster:ClusterGraph = {

    val nodesAndNodes = testGraph.nodes.map(n => (n,Leaf(n)))
    val nodeMap = nodesAndNodes.toMap


    def edgeFromEdge(e:testGraph.OuterEdgeType): (NodePair[Leaf[String]], Int) = {
      (NodePair(nodeMap(e._1._1),nodeMap(e._1._2)),1)    //todo use original weights
    }

    val edges = testGraph.edges.map(edgeFromEdge)

    //todo make Node and Edge types covariant
    val result:ClusterGraph = AdjacencyLabelUndigraph[Cluster,Int](edges,nodes = nodesAndNodes.map(n => n._2),noEdgeExistsValue = 0)

    result
  }

  //The core data structure is a Set of (Node -> Map(Node -> Weight)) , an adjacency graph
  //The resulting type of each iteration is the same Set of (Node -> Map(Node -> Weight, but the node type here is a Set of the previous iteration's Nodes. (recursive type). A cluster label will help preserve sanity

  // 0) Initialize a Cluster for each node
/*
  def initialCluster(undiGraph: LabelUndigraph[String,Int]) = {
    val graph:Graph = undiGraph.innerNodes.map(n => Set(n.value) -> n.edges.map(e => Set(e._1.other(n).value) -> e._2))

    graph
  }



  def sumOfWeights(graph:Graph):Int = {
    graph.map(c => c._2.map(_._2).sum).sum
  }

  def modularity(cluster:Cluster,graph: Graph) = {

  }
    */
  // 1) For each node, chose (create) the cluster that delivers the biggest modularity gain. Produce (Node -> Cluster). (Label the cluster with the first node in it if you need to)



  // 2) Use the clusters to rebuild the next iteration's graph - a Set of (Node -> Map(Node -> Weight))


}
