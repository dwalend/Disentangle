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


  def relaxSource[Node,Label,Key](digraph:IndexedDigraph[Node,Label],
                                  labels:ArrayBuffer[Label],
                                  semiring:SemiringSupport[Label,Key]#Semiring)
                                (from:digraph.InnerNodeType,
                                 through:digraph.InnerNodeType,
                                 to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = labels(through.index)
    val throughTo:Label = digraph.edge(through,to)

    val current:Label = labels(to.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm.
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
      //todo if you can get successors and edges in one call, you won't need the edge() call in relax, and can avoid building the edge matrix
      for(successor <- topNode.successors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = heapMembers(successor.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSource(initialGraph,labels,support.semiring)(source,topNode,successor)
          labels(successor.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labels.zipWithIndex.map(x => (source.value,initialGraph.node(x._2),x._1)).filter(x => x._3 != support.semiring.O)
  }

  def allPairsShortestPaths[Node,Label,Key](labelDigraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key]):Seq[(Node,Node,Label)] = {

    labelDigraph.innerNodes.map(source => dijkstraSingleSource(labelDigraph,support)(source)).flatten
  }

  def relaxSink[Node,Label,Key](digraph:IndexedDigraph[Node,Label],
                                labels:ArrayBuffer[Label],
                                semiring:SemiringSupport[Label,Key]#Semiring)
                               (from:digraph.InnerNodeType,
                                through:digraph.InnerNodeType,
                                to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = digraph.edge(from,through)
    val throughTo:Label = labels(through.index)

    val current:Label = labels(from.index)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * Dijkstra's algorithm for a single sink. This supports a heap argument to enable Brandes' algorithm. private to the semiring package to keep people out of trouble.
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
        val heapKey = heapMembers(predecessor.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSink(initialGraph,labels,support.semiring)(predecessor,topNode,sink)
          labels(predecessor.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labels.zipWithIndex.map(x => (initialGraph.node(x._2),sink.value,x._1))
  }

  /**
   * Dijkstra's algorithm for a single sink.
   */
  def dijkstraSingleSink[Node,Label,Key](initialDigraph:IndexedDigraph[Node,Label],
                                         support:SemiringSupport[Label,Key])
                                        (sink:initialDigraph.InnerNodeType):Seq[(Node,Node,Label)] = {
    val heap:Heap[Key,initialDigraph.InnerNodeType] = new FibonacciHeap[Key,initialDigraph.InnerNodeType](support.heapOrdering)
    dijkstraSingleSinkCustomHeap(initialDigraph,support)(sink,heap).filter(x => x._3 != support.semiring.O)
  }

}

