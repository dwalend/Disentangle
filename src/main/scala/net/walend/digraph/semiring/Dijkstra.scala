package net.walend.digraph.semiring

import net.walend.heap.{FibonacciHeap, Heap}
import scala.collection.mutable.ArrayBuffer
import net.walend.digraph.IndexedDigraph

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
  def relaxSource[Node,Label,Key](digraph:IndexedDigraph[Node,Label],
                                  labels:ArrayBuffer[Label],
                                  semiring:SemiringSupport[Label,Key]#Semiring)
                                (from:digraph.InnerNodeType,
                                 through:digraph.InnerNodeType,
                                 to:(digraph.InnerNodeType,digraph.InnerNodeType,Label)):Label = {

    val fromThrough:Label = labels(through.index)
    val throughTo:Label = to._3

    val current:Label = labels(to._2.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm.
   *
   * O(n ln(n) + e)
   */
  def dijkstraSingleSource[Node,Label,Key](initialGraph:IndexedDigraph[Node,Label],
                                           support:SemiringSupport[Label,Key])
                                          (source:initialGraph.InnerNodeType):Seq[(Node,Node,Label)] = {
    //Set up the map of Nodes to HeapKeys
    val labels:ArrayBuffer[Label] = ArrayBuffer.fill(initialGraph.nodes.size)(support.semiring.O)

    val heap:Heap[Key,initialGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)

    val heapMembers:IndexedSeq[heap.HeapMember] = initialGraph.innerNodes.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))
    
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
        val heapKey = heapMembers(successor._2.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSource(initialGraph,labels,support.semiring)(source,topNode,successor)
          labels(successor._2.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labels.zipWithIndex.map(x => (source.value,initialGraph.node(x._2),x._1)).filter(x => x._3 != support.semiring.O)
  }

  /**
   * O(n^2 ln(n) + ne)
   */
  def allPairsShortestPaths[Node,Label,Key](labelDigraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key]):Seq[(Node,Node,Label)] = {

    labelDigraph.innerNodes.map(source => dijkstraSingleSource(labelDigraph,support)(source)).flatten
  }

  /**
   * Create a digraph of Labels from an edge list.
   *
   * O(n ln(n) + e ln(n))
   *
   * @return an IndexedDigraph with all nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def createLabelDigraph[Node,Edge,Label,Key](edges:Seq[(Node,Node,Edge)] = Seq.empty,
                                              extraNodes:Seq[Node] = Seq.empty,
                                              support:SemiringSupport[Label,Key],
                                              labelForEdge:(Node,Node,Edge)=>Label):IndexedDigraph[Node,Label] = {

    val nodes = (extraNodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct
    val nonSelfEdges = edges.filter(x => x._1 != x._2)
    val labelEdges = nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfEdges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    import net.walend.digraph.AdjacencyDigraph
    AdjacencyDigraph(labelEdges,nodes,support.semiring.O)
  }

  /**
   * O(n^2 ln(n) + ne)
   */
  def allPairsShortestPaths[Node,Edge,Label,Key](edges:Seq[(Node,Node,Edge)] = Seq.empty,
                                            extraNodes:Seq[Node] = Seq.empty,
                                            support:SemiringSupport[Label,Key],
                                            labelForEdge:(Node,Node,Edge)=>Label):Seq[(Node,Node,Label)] = {
    val labelDigraph = createLabelDigraph(edges,extraNodes,support,labelForEdge)
    labelDigraph.innerNodes.map(source => dijkstraSingleSource(labelDigraph,support)(source)).flatten
  }

  /**
   * O(1)
   */
  def relaxSink[Node,Label,Key](digraph:IndexedDigraph[Node,Label],
                                labels:ArrayBuffer[Label],
                                semiring:SemiringSupport[Label,Key]#Semiring)
                               (from:(digraph.InnerNodeType,digraph.InnerNodeType,Label),
                                through:digraph.InnerNodeType,
                                to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = from._3
    val throughTo:Label = labels(through.index)

    val current:Label = labels(from._1.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm for a single sink. This supports a heap argument to enable Brandes' algorithm. private to the semiring package to keep people out of trouble.
   *
   * O(n ln(n) + e)
   */
  //todo could not use default argument for the heap. Report that as a possible bug.
  private[semiring] def dijkstraSingleSinkCustomHeap[Node,Label,Key](initialGraph:IndexedDigraph[Node,Label],
                                                   support:SemiringSupport[Label,Key])
                                                  (sink:initialGraph.InnerNodeType,
                                                   heap:Heap[Key,initialGraph.InnerNodeType]):Seq[(Node,Node,Label)] = {
    //Set up the map of Nodes to HeapKeys
    val labels:ArrayBuffer[Label] = ArrayBuffer.fill(initialGraph.nodes.size)(support.semiring.O)

    val heapMembers:IndexedSeq[heap.HeapMember] = initialGraph.innerNodes.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))

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
        val heapKey = heapMembers(predecessor._1.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSink(initialGraph,labels,support.semiring)(predecessor,topNode,sink)
          labels(predecessor._1.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labels.zipWithIndex.map(x => (initialGraph.node(x._2),sink.value,x._1))
  }

  /**
   * Dijkstra's algorithm for a single sink.
   *
   * O(n ln(n) + e)
   */
  def dijkstraSingleSink[Node,Label,Key](initialDigraph:IndexedDigraph[Node,Label],
                                         support:SemiringSupport[Label,Key])
                                        (sink:initialDigraph.InnerNodeType):Seq[(Node,Node,Label)] = {
    val heap:Heap[Key,initialDigraph.InnerNodeType] = new FibonacciHeap[Key,initialDigraph.InnerNodeType](support.heapOrdering)
    dijkstraSingleSinkCustomHeap(initialDigraph,support)(sink,heap).filter(x => x._3 != support.semiring.O)
  }
}

