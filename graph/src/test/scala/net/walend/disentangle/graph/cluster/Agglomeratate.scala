package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{AdjacencyUndigraph, IndexedUndigraph, SomeGraph, NodePair}

import scala.collection.{GenTraversable, GenSeq, GenSet}

/**
  * Create a hierarchy of cluster graphs via parallel agglomeratation based on the Jaccard index for each node.
  * Exploits bit-wise operations and parallelism
  *
  * @see https://en.wikipedia.org/wiki/Hierarchical_clustering
  * @author dwalend
  * @since v0.2.1
  */

/*
Phase 3 - merge clusters into a new generation of clusters in a new graph

(I think this can be done in parallel per cluster with the full map of where everything is going)

Groupby on that map.
For each Set of clusters, create a new Cluster that contains those clusters.
  For every edge completely inside the cluster, keep that edge in the subgraph
  For every edge that spans from inside the cluster to outside, add an edge from the new cluster to the (new) outside cluster

Create a Graph from these new Clusters and spanning edges. Isolates just pass through.

phase 4 - Try with a smaller graph.

Go to phase 2 with this new graph unless it has no edges.

-----

Worst case is O(n ln n). (I think) Each node will be merged with some other node unless it is an isolate (which should just drop out of the problem).


 */

object Agglomeratate {

/*
  type Node = String
  type Weight = Double

  type Edge = (Cluster,Weight)

  type Cluster = Set[Node]
*/
//  type Graph = GenSet[(Cluster,Set[Edge])]


//  case class Graph(subgraphs:Set[Graph],edges:Set[(NodePair[Graph],Edge)])

  sealed abstract class Cluster {
//    def edges:Map[Cluster,Double]
  }
  type ClusterGraph = IndexedUndigraph[Cluster]

  case class Leaf[Node](node:Node) extends Cluster

  case class Subgraph(graph:ClusterGraph) extends Cluster  //todo replace Double with something from a Semiring, maybe

//  type Edge = ClusterGraph.OuterEdgeType
//  case class Cluster(graph:LabelUndigraph[Cluster,Double],archNode:String)

//  val graph = LabelUndigraph()

  /*
  Phase 0 - init

  For a graph create a Leaf - a cluster of one node in the graph. Build up an initial graph of Clusters (that are leafs)
  Edges in this graph indicate existence of an edge in the original graph.

    A Cluster is either a Leaf (contains one node) or a Subgraph (contains a graph of Clusters and edges between them)

  Creating the Leafs should be fine in parallel

  The initial graph is a Map(Leaf -> Set(Leaf)) of type Map(Cluster -> Set(Cluster))s that it can reach
*/

  val testGraph = SomeGraph.testUndigraph//todo work with the karate school graph

  def leafClusterFromGraph[Node](graph:IndexedUndigraph[Node]):ClusterGraph = {

    val nodesAndNodes = graph.nodes.asSeq.map(n => (n,Leaf(n)))
    val nodeMap = nodesAndNodes.toMap


    def edgeFromEdge(e:graph.OuterEdgeType): NodePair[Leaf[Node]] = {
      NodePair(nodeMap(e._1),nodeMap(e._2))    //todo use original weights
    }

    val edges = graph.edges.map(edgeFromEdge)

    AdjacencyUndigraph[Cluster](edges,nodes = nodesAndNodes.map(n => n._2).toSeq)
  }

  val leafCluster: ClusterGraph = leafClusterFromGraph(testGraph)

/*
  Phase 1 - sort the nodes by /something/ , best bet is either most-to-least or least-to-most edges .

  parallel sort should do fine . (In something too big to sort, just sort the neighbors)
*/
  def sortNodes(graph:ClusterGraph): List[graph.InnerNodeType] = graph.innerNodes.to[List].sortBy(_.edges.size).reverse

  val sortedLeafs = sortNodes(leafCluster)

/*
Phase 2 - pick most similar Cluster for each Cluster

(can be in parallel)
For each node
  From its edges
    Pick the node with the highest Jaccard index that isn't the node you're considering
      Break ties using the sort order

Gives a Map(Cluster -> Cluster)

(has to gather, but can be remarked in parallel if this is a bottle neck)
If two nodes point to each other then all nodes that point to either of those two should be merged. Relabel to whichever comes first in the sort order
If a node is an isolate - max Jaccard index is Zero, just point it to itself or dump it in an isolate bucket.

Map(Cluster -> Cluster marker to merge with for next generation)

*/
  def pickCharacteristicCluster(graph:ClusterGraph):Map[Cluster,Cluster] = {

    graph.innerNodes.map(n => (n.value,n.value)).toMap //start here

  }



  /**
    * Merge all the clustersToMerge Doubleo a new Cluster.
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
      nodes.contains(edge._1) && nodes.contains(edge._2)
    }

    val (edgesInMergedCluster,otherEdges: GenTraversable[NodePair[Cluster]]) = clusterGraph.edges.partition(e => completelyContainedEdge(e,clustersToMerge))
    val (edgesNotMerged,spanningEdges: GenTraversable[NodePair[Cluster]]) = otherEdges.partition(e => completelyContainedEdge(e,clustersNotMerged))

    val mergedCluster = Subgraph(AdjacencyUndigraph[Cluster](edges = edgesInMergedCluster,nodes = clustersToMerge.to[GenSeq]))

    def outsideNodeOfSpanningEdge(spanningEdge:clusterGraph.OuterEdgeType):Cluster = {
      if (clustersToMerge.contains(spanningEdge._1)) spanningEdge._2
      else spanningEdge._1
    }
    val edgesToMergedCluster: GenSeq[NodePair[Cluster]] = spanningEdges.map(e => e -> outsideNodeOfSpanningEdge(e)).groupBy(x => x._2).map(x => (x._1,x._2.map(y => y._1._2).size)).map(x => NodePair(x._1,mergedCluster)).toSeq

    AdjacencyUndigraph[Cluster](edges = edgesNotMerged ++ edgesToMergedCluster,
                                          nodes = clustersNotMerged.toSeq :+ mergedCluster)
  }
        /*
  //use for Sigma_in, Sigma_tot, 2m
  //todo maybe make a lazy val in Cluster
  def totalWeight(graph: ClusterGraph):Double = graph.edges.map(e => e._2).sum

  def totalWeight(cluster:Cluster):Double = cluster match {
    case subgraph:Subgraph => totalWeight(subgraph.graph)
    case leaf:Leaf => 0
  }

  //use for sigmaTot, k_i
  //todo makey make a lazy val on the inner node
  def totalIncidentWeight(graph:ClusterGraph,cluster:Cluster):Double = {
    val innerNode: graph.InnerNodeType = graph.innerNode(cluster).getOrElse(throw new IllegalArgumentException(s"$cluster is not in $graph"))
    innerNode.edges.map(e => e._2).sum
  }

  //use for k_i_in
  def incidentWeightBetweenClusters(graph: ClusterGraph,a:Cluster,b:Cluster):Double = graph.edge(NodePair(a,b))._2
  
  //todo maybe do something stupider with set intersections as a faster algorithm

  def deltaModularity(graph:ClusterGraph,existing:Cluster,proposed:Cluster):Double = {
    val twoM = totalWeight(graph)
    val sigmaIn = totalWeight(existing)
    val kiIn = incidentWeightBetweenClusters(graph,existing,proposed)
    val sigmaTot = totalIncidentWeight(graph,existing)
    val ki = totalIncidentWeight(graph,proposed)

    def sqr(x:Double) = Math.pow(x,2)

    //todo since all comparisons are relative anyway it might make sense to factor out twoM, then use Ints or other numeric types
    val firstTerm = ((sigmaIn+kiIn)/twoM) - sqr((sigmaTot + ki)/twoM)
    val secondTerm = (sigmaIn/twoM) - sqr(sigmaTot/twoM) - sqr(ki/twoM)
    firstTerm - secondTerm
  }

  def bestModularityGainWith(graph:ClusterGraph,cluster:Cluster):Cluster = {
    val innerNode: graph.InnerNodeType = graph.innerNode(cluster).getOrElse(throw new IllegalArgumentException(s"$cluster is not in $graph"))
    val clustersAndDeltaMods: Set[(Cluster, Double)] = innerNode.edges.map(e => {
      val proposed = e._1.other(innerNode).value
      val deltaMod = deltaModularity(graph,cluster,proposed)
      (proposed,deltaMod)
    })
    if(clustersAndDeltaMods.isEmpty) cluster
    else {
      val bestCluster: (Cluster, Double) = clustersAndDeltaMods.maxBy(_._2)
      if(bestCluster._2 >= 0) bestCluster._1
      else cluster
    }
  }

  def bestModularitiesWith(graph:ClusterGraph): GenSet[(Cluster, Cluster)] = graph.nodes.map(n => (n,bestModularityGainWith(graph,n)))

  def findClustersAndMerge(graph:ClusterGraph):ClusterGraph = {
    val bestMods: GenSet[(Cluster, Cluster)] = bestModularitiesWith(graph)
    val clustersToMerge: GenMap[Cluster, GenSet[Cluster]] = bestMods.groupBy(_._2).map(x => (x._1,x._2.map(_._1)))
    val (loneClusters,groupedClusters: GenMap[Cluster, GenSet[Cluster]]) = clustersToMerge.partition(_._2.size == 1)

    //if a lone cluster is a key then add it to that key's value
    val (loneClustersToPutSomewhere,survivors) = loneClusters.partition(x => groupedClusters.contains(x._1))

    //todo use a foldLeft ? to merge clusters

  }
   */
  //The core data structure is a Set of (Node -> Map(Node -> Weight)) , an adjacency graph
  //The resulting type of each iteration is the same Set of (Node -> Map(Node -> Weight, but the node type here is a Set of the previous iteration's Nodes. (recursive type). A cluster label will help preserve sanity

  // 0) Initialize a Cluster for each node
/*
  def initialCluster(undiGraph: LabelUndigraph[String,Double]) = {
    val graph:Graph = undiGraph.innerNodes.map(n => Set(n.value) -> n.edges.map(e => Set(e._1.other(n).value) -> e._2))

    graph
  }



  def sumOfWeights(graph:Graph):Double = {
    graph.map(c => c._2.map(_._2).sum).sum
  }

  def modularity(cluster:Cluster,graph: Graph) = {

  }
    */
  // 1) For each node, chose (create) the cluster that delivers the biggest modularity gain. Produce (Node -> Cluster). (Label the cluster with the first node in it if you need to)



  // 2) Use the clusters to rebuild the next iteration's graph - a Set of (Node -> Map(Node -> Weight))


}
