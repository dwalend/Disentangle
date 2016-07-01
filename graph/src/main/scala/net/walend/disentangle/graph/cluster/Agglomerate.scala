package net.walend.disentangle.graph.cluster

import net.walend.disentangle.graph.{AdjacencyUndigraph, IndexedUndigraph, NodePair}

import scala.annotation.tailrec
import scala.collection.immutable.Iterable

/**
  * Create a hierarchy of graphs of clusters via parallel agglomeratation based on the Jaccard index for each node.
  *
  * Worst case is O(n ln n) to cluster the whole graph. Each node will be merged with some other node unless it is an isolate (which should just drop out of the problem).
  *
  * @see https://en.wikipedia.org/wiki/Hierarchical_clustering
  * @author dwalend
  * @since v0.2.1
  */

//todo exploit bitwise operations and parallelism
//todo make it possible to substitute something with labels for the Jaccard index
object Agglomerate {

  //todo trait with Scala 2.12
  sealed abstract class Cluster(val generation: Int) {
    def members: Set[Cluster]
  }

  type ClusterGraph = IndexedUndigraph[Cluster]

  /**
    * A cluster with exactly one member.
    */
  case class Initial[Node](node:Node) extends Cluster(1) {
    override val members: Set[Cluster] = Set.empty

    override def toString = node.toString
  }

  /**
    * A cluster isolated from other clusters
    */
  case class Isolates(members:Set[Cluster],override val generation: Int) extends Cluster(generation)

  //todo is there a way to show a picture in Scaladoc?
  /**
    * A cluster where all members are most like a cluster outside their membership.
    */
  case class Sibling(graph:ClusterGraph, archetype:Cluster,override val generation:Int) extends Cluster(generation) {
    override def members: Set[Cluster] = graph.nodes
  }

  /**
    * A cluster where all members (but one) are most like a single cluster within their membership. That one cluster is most like another member cluster.
    */
  case class Wheel(graph:ClusterGraph, hub:Cluster,override val generation:Int) extends Cluster(generation) {
    override def members: Set[Cluster] = graph.nodes
  }

  /**
    * A cluster with members that form a loop.
    */
  case class Cycle(graph:ClusterGraph, cycle:Seq[Cluster],override val generation:Int) extends Cluster(generation) {
    override def members: Set[Cluster] = graph.nodes
  }

  /**
    * A cluster with members that form a chain.
    */
  //TODO I am curious about length 1 Caterpillars. They violate my scale-up rule, but I'm not sure if they cause actual problems. Plan is to leave them in for now, see if they get swept up neatly in the next iteration.
  case class Caterpillar(graph:ClusterGraph, nodes:Seq[Cluster],override val generation:Int) extends Cluster(generation) {
    override def members: Set[Cluster] = graph.nodes
  }

  /**
    * Phase 0 - init
    **
    * For a graph create an Initial cluster - a cluster of one node in the graph. Create a graph of Initial Clusters
    * Edges in this graph indicate existence of an edge in the original graph.
    **
    * Creating the Initial clusters should be fine in parallel
    **
    * The initial graph is a Map(Initial -> Set(Initial)) of type Map(Cluster -> Set(Cluster))s that it can reach
    */
  def initialClusterFromGraph[Node](graph:IndexedUndigraph[Node]):ClusterGraph = {

    val nodesToInitialClusters = graph.nodes.asSeq.map(n => (n,Initial(n)))
    val nodeMap = nodesToInitialClusters.toMap

    def clusterEdgeFromEdge(e:graph.OuterEdgeType): NodePair[Initial[Node]] = {
      NodePair(nodeMap(e._1),nodeMap(e._2))    //todo original weights - will need a mapping function at some point.
    }

    val edges = graph.edges.map(clusterEdgeFromEdge)

    AdjacencyUndigraph[Cluster](edges,nodes = nodesToInitialClusters.map(n => n._2))
  }

  /**
    * Phase 1 - sort the nodes by /something/ , best bet is either most-to-least or least-to-most edges.  .
    **
    * parallel sort should do fine . (In something too big to sort, just sort the neighbors)
    */
  def sortNodes(graph:ClusterGraph): List[graph.InnerNodeType] = graph.innerNodes.to[List].sortBy(_.innerEdges.size).reverse

  /**
    * Phase 2 - pick most similar Cluster to each Cluster
    **
    * (can be in parallel)
    * For each node
    * From its edges
    * Partition isolates out into their own cluster
    * Pick the node with the highest Jaccard index that isn't the node you're considering
    * Break ties using the sort order
    **
    * Gives a Map(Cluster -> Cluster)
    **
    * (has to gather, but can be remarked in parallel if this is a bottle neck)
    * If a node is an isolate - max Jaccard index is Zero, just point it to itself or dump it in an isolate bucket.
    **
    * Map(Cluster -> Cluster marker to merge with for next generation)
    *
    */
  def pickCharacteristicClusters(graph:ClusterGraph): (Map[Cluster, Cluster], Option[FormIsolate]) = {

    /**
      * @see https://en.wikipedia.org/wiki/Jaccard_index
      */
    def jaccardIndex(node:graph.InnerNodeType,candidate:graph.InnerNodeType): Int = {
      node.innerEdges.union(candidate.innerEdges).size
    }

    //todo someday pass in weights
    def nodeWithMaxJaccardIndex(node:graph.InnerNodeType):graph.InnerNodeType = {
      //skip self-edges, which will have a maximum jaccard index
      node.innerEdges.filterNot(_.selfEdge).map(e => e.other(node)).maxBy(jaccardIndex(node,_))
    }

    //separate connected nodes from isolates
    val (connected,isolated) = graph.innerNodes.partition(n => n.innerEdges.nonEmpty)

    val clustersToMostSimilarNeighbor = connected.map(n => (n.value,nodeWithMaxJaccardIndex(n).value)).toMap
    val isolates = if(!isolated.isEmpty) Some(FormIsolate(isolated.map(_.value)))
    else None

    (clustersToMostSimilarNeighbor,isolates)
  }

  /**
    * Phase 3 - Refine the set of characteristic clusters and deal with corner cases
    **
    * Join every cluster with at least one other cluster. (Worst case is a binary tree) Process at least n clusters per n units of work.
    **
    * Make a decision for each node as follows (start at the top of the list):
    **
    * Create a Siblings cluster from all of the nodes that selected the same "most similar node" if there is more than one.
    * Nodes that remain picked unique targets. Each target will have one node that selected it.
    * Create a Wheel from any node that was selected by a Siblings cluster and picked a node in those Siblings
    * Find Cycles and Caterpillars by following connections.
    * For Caterpillars, the start of a Caterpillars won't be in the set of picked clusters. Find all the starts and follow them to form caterpillars. (Caterpillars must end on siblings or wheels. )
    **
    * For a Cycle, start at a node and follow downstream to detect, building a list of nodes.
    * If you hit the start node, you've found a complete Cycle.
    **
    * (Expect time-dependent relationships to have a big caterpillar, others to have Cycles and Wheels)
    *
    */
  //todo maybe these are better as apply() methods on Cluster's companion objects ???
  trait FormCluster {
    def members:Set[Cluster]

    def clusterGraphAndExternalEdges(prevGraph: ClusterGraph): (ClusterGraph, Set[prevGraph.OuterEdgeType]) = {
      val involvedEdges: Set[prevGraph.OuterEdgeType] = members.flatMap(cluster => prevGraph.innerNode(cluster).get.outerEdges)

      def isInternalEdge(edge:prevGraph.OuterEdgeType):Boolean =
        members.contains(edge._1) && members.contains(edge._2)

      val (internalEdges,externalEdges) = involvedEdges.partition(isInternalEdge)

      (AdjacencyUndigraph[Cluster](edges = internalEdges.map(e => NodePair(e._1,e._2)),
        nodes = members.to[Seq])
        ,externalEdges)
    }

    def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Cluster,Set[prevGraph.OuterEdgeType])
  }

  def firstMember[A](members:Set[A]):A = members.iterator.next()

  case class FormIsolate(members:Set[Cluster]) extends FormCluster {
    override def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Isolates,Set[prevGraph.OuterEdgeType])  = {
      val (clusterGraph,externalEdges) = clusterGraphAndExternalEdges(prevGraph)
      (Isolates(clusterGraph.nodes,firstMember(members).generation + 1 ),externalEdges)
    }
  }

  case class FormSibling(archType:Cluster, members:Set[Cluster]) extends FormCluster {
    override def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Sibling,Set[prevGraph.OuterEdgeType])  = {
      val (clusterGraph,externalEdges) = clusterGraphAndExternalEdges(prevGraph)
      (Sibling(clusterGraph,archType,firstMember(members).generation + 1),externalEdges)
    }
  }

  case class FormWheel(archType:Cluster, members:Set[Cluster]) extends FormCluster {
    override def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Wheel,Set[prevGraph.OuterEdgeType])  = {
      val (clusterGraph,externalEdges) = clusterGraphAndExternalEdges(prevGraph)
      (Wheel(clusterGraph,archType,firstMember(members).generation + 1),externalEdges)
    }
  }

  case class FormCaterpillar(memberList:Seq[Cluster]) extends FormCluster {
    lazy val members: Set[Cluster] = memberList.to[Set]

    override def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Caterpillar,Set[prevGraph.OuterEdgeType])  = {
      val (clusterGraph,externalEdges) = clusterGraphAndExternalEdges(prevGraph)
      (Caterpillar(clusterGraph,memberList,firstMember(members).generation + 1),externalEdges)
    }
  }

  case class FormCycle(memberList:List[Cluster]) extends FormCluster {
    lazy val members: Set[Cluster] = memberList.to[Set]

    override def toClusterAndExternalEdges(prevGraph: ClusterGraph):(Cycle,Set[prevGraph.OuterEdgeType])  = {
      val (clusterGraph,externalEdges) = clusterGraphAndExternalEdges(prevGraph)
      (Cycle(clusterGraph,memberList,firstMember(members).generation + 1),externalEdges)
    }
  }

  def clustersFromMostSimilar(clustersToPicked:Map[Cluster,Cluster]): Iterable[FormCluster] = {

    //have to gather to do the groupBy. Dang.
    val pickedClustersToSets: Map[Cluster, Set[Cluster]] = clustersToPicked.groupBy(c2c => c2c._2).map(c2cmap => (c2cmap._1,c2cmap._2.keySet))
    //Wheels and Siblings first
    val (wheelsOrSiblings,pathLikeThings) = pickedClustersToSets.partition(x => x._2.size > 1) //Set size is >= 1

    //The hub of a wheel might be in pathLikeThings, or something else's sibling, so it isn't actually part of a wheel.
    //todo straighten out the above later, or ignore it
    //todo something in this is not quite working as expected with the test case
    val (wheelParts,siblingParts) = wheelsOrSiblings.partition(pToCS => pToCS._2.contains(clustersToPicked(pToCS._1)))
    val siblings: Iterable[FormSibling] = siblingParts.map(x=>FormSibling(x._1,x._2))
    val wheels: Iterable[FormSibling] = wheelParts.map(x=>FormSibling(x._1,x._2))

    val pathLikePicksToClusters: Map[Cluster, Cluster] = pathLikeThings.map(pToCS => pToCS._1 -> pToCS._2.find(x => true).get) //Set size is 1, so the find() is OK.

    val pathLikeClustersToPicks: Map[Cluster, Cluster] = pathLikePicksToClusters.map(_.swap)

    //Find starts of chains
    val chainStarts: Set[Cluster] = pathLikePicksToClusters.filterNot(x => pathLikePicksToClusters.keySet.contains(x._2)).values.to[Set]  //need this as a Set??

    //follow the chains up
    def createChain(link:Cluster):List[Cluster] = {
      pathLikeClustersToPicks.get(link).fold(List.empty[Cluster])(next => next :: createChain(next))
    }

    val chains: Set[FormCaterpillar] = chainStarts.map(createChain).map(FormCaterpillar)
    //todo find size = 1 chains and put these warts in the cluster that they point to (to preserve the "always combine one node with at least one other) if it needs to be done

    val clustersInChains: Set[Cluster] = chains.flatMap(_.memberList)

    // anything left must be part of a loop

    val clustersInLoops: Map[Cluster, Cluster] = pathLikeClustersToPicks.filterNot(cToP => clustersInChains.contains(cToP._1))

    //unfortunately I don't see a good way to do this in parallel without doing extra work. (building the same loop multiple times, then squashing them into a set. Hopefully there won't be many of these early. It seems unlikely in a typical social hairball
    @tailrec
    def createCycle(loopClustersToPicks:Map[Cluster,Cluster],acc:List[List[Cluster]] = Nil):List[List[Cluster]] = {

      @tailrec
      def followCycle(start:Cluster,cluster:Cluster,remaining:Map[Cluster,Cluster],acc:List[Cluster] = Nil):List[Cluster] = {
        val next = remaining.get(cluster).get
        if (next == start) cluster::acc
        else followCycle(start,next,remaining,cluster::acc)
      }

      if(loopClustersToPicks.isEmpty) acc
      else {
        val start = loopClustersToPicks.iterator.next()._1
        val aLoop = followCycle(start, start, loopClustersToPicks)
        val loopAsSet = aLoop.to[Set]
        val remainingLessALoop = loopClustersToPicks.filterNot(x => loopAsSet.contains(x._1))
        createCycle(remainingLessALoop,aLoop::acc)
      }
    }
    val loops: List[FormCycle] = createCycle(clustersInLoops).map(FormCycle)

    wheels ++ siblings ++ chains ++ loops
  }

  /**
    * Phase 4 - merge clusters into a new generation of clusters in a new graph
    *
    * (I think this can be done in parallel per cluster with the full map of where everything is going)
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
  def merge(clusterGraph: ClusterGraph,clustersToMerge:Iterable[FormCluster]):ClusterGraph = {

    //todo is it faster to let toClusterAndExternalEdges find all the external edges, or to find all edges - all internal edges?
    //todo I'll guess there will be more internal edges due to max similarity
    val mergedClustersAndExternalEdges = clustersToMerge.map(c => c.toClusterAndExternalEdges(clusterGraph))
    val nodes = mergedClustersAndExternalEdges.map(mcaee => mcaee._1).to[Seq]

    val membersToMerged: Map[Cluster, Cluster] = mergedClustersAndExternalEdges.flatMap(mc => mc._1.members.map(m => (m,mc._1))).toMap

    val edges = mergedClustersAndExternalEdges.flatMap(mcaxe => mcaxe._2).map(prev => NodePair(membersToMerged(prev._1),membersToMerged(prev._2))).to[Set] //todo if using weighted edges, use .map(), then merge the edges with something other than .to[Set]

    AdjacencyUndigraph(edges,nodes)
  }

  /**
    * Create a generation of clusters from the previous generation.
    */
  def createClusters(graph:ClusterGraph):ClusterGraph = {
    val sortedInitialNodes = sortNodes(graph) //todo this isn't used yet  !!
    val (clustersToMostSimilarNeighbor,isolates) = pickCharacteristicClusters(graph)
    val formClusters: Iterable[FormCluster] = clustersFromMostSimilar(clustersToMostSimilarNeighbor) ++ isolates.to[Seq]
    merge(graph,formClusters)
  }

  /**
    * Keep creating levels of clusters until there is only one isolated node.
    */
  def agglomerate(graph:ClusterGraph):List[ClusterGraph] = {
    if (graph.nodeCount <= 1) List(graph)
    else {
      val nextGraph = createClusters(graph)
      graph :: agglomerate(nextGraph)
    }
  }
}

