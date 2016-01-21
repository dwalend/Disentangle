package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{LabelUndigraph, IndexedLabelUndigraph, SomeGraph, AdjacencyLabelUndigraph, NodePair}

import scala.collection.{GenMap, GenSeq, GenSet}

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

  sealed abstract class Cluster {
//    def edges:Map[Cluster,Int]
  }

  case class Leaf[Node](node:Node) extends Cluster

  type ClusterGraph = LabelUndigraph[Cluster,Int]

  case class Subgraph(graph:ClusterGraph) extends Cluster  //todo replace Int with something from a Semiring, maybe

//  type Edge = ClusterGraph.OuterEdgeType
//  case class Cluster(graph:LabelUndigraph[Cluster,Int],archNode:String)

//  val graph = LabelUndigraph()

  val testGraph = SomeGraph.testUndigraph//todo work with the karate school graph

  def leafClusterFromGraph[Node,Label](graph:IndexedLabelUndigraph[Node,Label]):ClusterGraph = {

    val nodesAndNodes = graph.nodes.asSeq.map(n => (n,Leaf(n)))
    val nodeMap = nodesAndNodes.toMap


    def edgeFromEdge(e:graph.OuterEdgeType): (NodePair[Leaf[Node]], Int) = {
      (NodePair(nodeMap(e._1._1),nodeMap(e._1._2)),1)    //todo use original weights
    }

    val edges = graph.edges.map(edgeFromEdge)

    AdjacencyLabelUndigraph[Cluster,Int](edges,nodes = nodesAndNodes.map(n => n._2).toSeq)
  }

  val leafCluster: ClusterGraph = leafClusterFromGraph(testGraph)

  /**
    * Merge all the clustersToMerge into a new Cluster.
    * Return a ClusterGraph that replaces all the merged clusters with a single new cluster and none of the clusters to merge, and retains all other clusters.
    *
    * The clustersToMerge become subclusters in the new cluster
    * Edges between clustersToMerge become edges in the new cluster
    * Edges not involved with clustersToMerge pass through as edges in the resulting ClusterGraph
    * Edges between the clustersToMerge and other clusters {if no edge exists yet} become new edges
    *                                                      or {if an edge exists already} combine weight with the existing edge
    *
    */
  def merge(clusterGraph: ClusterGraph,clustersToMerge:Set[Cluster]):ClusterGraph = {

    val clustersNotMerged: GenSet[Cluster] = clusterGraph.nodes.partition(n => clustersToMerge.contains(n))._2

    def completelyContainedEdge(edge:clusterGraph.OuterEdgeType,nodes:GenSet[Cluster]):Boolean = {
      nodes.contains(edge._1._1) && nodes.contains(edge._1._2)
    }

    val (edgesInMergedCluster,otherEdges) = clusterGraph.edges.partition(e => completelyContainedEdge(e,clustersToMerge))
    val (edgesNotMerged,spanningEdges) = otherEdges.partition(e => completelyContainedEdge(e,clustersNotMerged))

    val mergedCluster = Subgraph(AdjacencyLabelUndigraph[Cluster,Int](edges = edgesInMergedCluster,nodes = clustersToMerge.to[GenSeq]))

    def outsideNodeOfSpanningEdge(spanningEdge:clusterGraph.OuterEdgeType):Cluster = {
      if (clustersToMerge.contains(spanningEdge._1._1)) spanningEdge._1._2
      else spanningEdge._1._1
    }
    val edgesToMergedCluster: GenSeq[(NodePair[Cluster], Int)] = spanningEdges.map(e => e -> outsideNodeOfSpanningEdge(e)).groupBy(x => x._2).map(x => (x._1,x._2.map(y => y._1._2).sum)).map(x => (NodePair(x._1,mergedCluster),x._2)).toSeq

    AdjacencyLabelUndigraph[Cluster,Int](edges = edgesNotMerged ++ edgesToMergedCluster,
      nodes = clustersNotMerged.toSeq :+ mergedCluster)
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
