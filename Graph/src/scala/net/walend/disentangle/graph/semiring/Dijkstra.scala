package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.IndexedLabelDigraph
import net.walend.disentangle.heap.{FibonacciHeap, Heap}

import scala.collection.mutable.ArrayBuffer

/**
 * An implementation of Dijkstra's algorithm for general graph minimization for both single-source and single-sink.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Dijkstra {

  /**
   * O(1)
   */
  def relaxSource[Node,Label,Key](digraph:IndexedLabelDigraph[Node,Label],
                                  labels:ArrayBuffer[Label],
                                  semiring:SemiringSupport[Label,Key]#Semiring)
                                (from:digraph.InnerNodeType,
                                 through:digraph.InnerNodeType,
                                 to:digraph.InnerEdgeType):Label = {

    val fromThrough:Label = labels(through.index)
    val throughTo:Label = to.label

    val current:Label = labels(to.to.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm.
   *
   * O(n ln(n) + e)
   */
  def dijkstraSingleSource[Node,Label,Key](initialGraph:IndexedLabelDigraph[Node,Label],
                                           support:SemiringSupport[Label,Key])
                                          (source:initialGraph.InnerNodeType):Seq[(Node,Node,Label)] = {
    //Set up the map of Nodes to HeapKeys
    val labels:ArrayBuffer[Label] = ArrayBuffer.fill(initialGraph.nodeCount)(support.semiring.O)

    val heap:Heap[Key,initialGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)

    //profiler said map was trouble without asSeq
    val heapMembers:IndexedSeq[heap.HeapMember] = initialGraph.innerNodes.asSeq.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))
    
    //Raise sourceInnerNode's to I
    labels(source.index) = support.semiring.I
    heapMembers(source.index).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //For any node that is reachable from this node not yet visited (because it's key is still in the heap)
      for(successor <- topNode.successors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = heapMembers(successor.to.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSource(initialGraph,labels,support.semiring)(source,topNode,successor)
          labels(successor.to.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    //put everything back together
    Seq.from(labels.zipWithIndex.map(x => (source.value,initialGraph.node(x._2),x._1)).filter(x => x._3 != support.semiring.O))
  }

  /**
   * O(n&#94;2 ln(n) + na)
   */
  def allNodesSingleSource[Node,Label,Key](labelDigraph:IndexedLabelDigraph[Node,Label],support:SemiringSupport[Label,Key]):Seq[(Node,Node,Label)] = {

    //The profiler pointed to both flatten and fold(IndexedSet.empty)((a,b) => a ++ b) as trouble.
    Seq.from(labelDigraph.innerNodes).flatMap(source => dijkstraSingleSource(labelDigraph, support)(source))
  }

  /**
   * Create a digraph of Labels from an edge list.
   *
   * O(n ln(n) + a ln(n))
   *
   * @return an IndexedDigraph with all nodes, a self-edge for each node with the semiring's identifier, and an edge for label edge specified by labelForEdge.
   */
  def createLabelDigraph[Node,EdgeLabel,Label,Key](edges: Iterable[(Node, Node, EdgeLabel)],
                                                   support: SemiringSupport[Label, Key],
                                                   labelForEdge: (Node, Node, EdgeLabel) => Label,
                                                   nodeOrder: Seq[Node] = Seq.empty):IndexedLabelDigraph[Node, Label] = {
    import net.walend.disentangle.graph.AdjacencyLabelDigraph

    val nodes = (nodeOrder ++ edges.map(_._1) ++ edges.map(_._2)).distinct
    val nonSelfEdges = edges.filter(x => x._1 != x._2)
    val labelEdges = nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfEdges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))
    AdjacencyLabelDigraph(labelEdges,nodes,support.semiring.O)
  }

  /**
   * O(n&#94;2 ln(n) + na)
   */
  def allPairsLeastPaths[Node,EdgeLabel,Label,Key](edges: Iterable[(Node, Node, EdgeLabel)],
                                                   support: SemiringSupport[Label, Key],
                                                   labelForEdge: (Node, Node, EdgeLabel) => Label,
                                                   nodeOrder: Seq[Node] = Seq.empty):Seq[(Node, Node, Label)] = {
    val labelDigraph = createLabelDigraph(edges, support, labelForEdge, nodeOrder)

    //profiler blamed both flatten and fold of IndexedSets as trouble
    Seq.from(labelDigraph.innerNodes).flatMap(source => dijkstraSingleSource(labelDigraph, support)(source))
  }

  def defaultSupport[Node]: AllPathsFirstSteps[Node, Int, Int] = AllPathsFirstSteps[Node,Int,Int](FewestNodes)

  def allPairsShortestPaths[Node,EdgeLabel](edges:Iterable[(Node,Node,EdgeLabel)],
                                          nodeOrder:Seq[Node] = Seq.empty
                                        ):Seq[(Node,Node,Option[FirstStepsTrait[Node, Int]])] = {
    val support = defaultSupport[Node]
    allPairsLeastPaths(edges, support, support.convertEdgeToLabel(FewestNodes.convertEdgeToLabel), nodeOrder)
  }

  /**
   * O(1)
   */
  def relaxSink[Node,Label,Key](digraph:IndexedLabelDigraph[Node,Label],
                                labels:ArrayBuffer[Label],
                                semiring:SemiringSupport[Label,Key]#Semiring)
                               (from:digraph.InnerEdgeType,
                                through:digraph.InnerNodeType,
                                to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = from.label
    val throughTo:Label = labels(through.index)

    val current:Label = labels(from.from.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm for a single sink.
   * This supports a heap argument to enable Brandes' algorithm.
   * private to the semiring package to keep people out of trouble.
   *
   * O(n ln(n) + a)
   */
  //todo could not use a default argument for the heap. Report that as a possible bug.
  private[semiring] def dijkstraSingleSinkCustomHeap[Node,Label,Key](initialGraph:IndexedLabelDigraph[Node,Label],
                                                   support:SemiringSupport[Label,Key])
                                                  (sink:initialGraph.InnerNodeType,
                                                   heap:Heap[Key,initialGraph.InnerNodeType]):IndexedSeq[(Node,Node,Label)] = {
    //Set up the map of Nodes to HeapKeys
    val labels:ArrayBuffer[Label] = ArrayBuffer.fill(initialGraph.nodeCount)(support.semiring.O)

    val heapMembers:IndexedSeq[heap.HeapMember] = initialGraph.innerNodes.asSeq.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))

    //Raise sourceInnerNode's to I
    labels(sink.index) = support.semiring.I
    heapMembers(sink.index).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()

      //For any node that can reach this node not yet visited (because it's key is still in the heap)
      for(predecessor <- topNode.predecessors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = heapMembers(predecessor.from.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSink(initialGraph,labels,support.semiring)(predecessor,topNode,sink)
          labels(predecessor.from.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    //don't filter out where labels == semiring.O. Need the indexes intact
    IndexedSeq.from(labels.zipWithIndex.map(x => (initialGraph.node(x._2),sink.value,x._1)))
  }

  /**
   * Dijkstra's algorithm for a single sink.
   *
   * O(n ln(n) + a)
   */
  def dijkstraSingleSink[Node,Label,Key](initialDigraph:IndexedLabelDigraph[Node,Label],
                                         support:SemiringSupport[Label,Key])
                                        (sink:initialDigraph.InnerNodeType):IndexedSeq[(Node,Node,Label)] = {
    val heap:Heap[Key,initialDigraph.InnerNodeType] = new FibonacciHeap[Key,initialDigraph.InnerNodeType](support.heapOrdering)
    dijkstraSingleSinkCustomHeap(initialDigraph,support)(sink,heap).filter(x => x._3 != support.semiring.O)
  }
}

