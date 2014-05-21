package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.{FibonacciHeap, Heap}

/**
 * An implementation of Dijkstra's algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Dijkstra {

  /**
   * Dijkstra's algorithm.
   */
  def dijkstraSingleSource[Node,Label,Key](labelGraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key])
                                          (source:labelGraph.InnerNodeType):Digraph[Node,Label] = {
    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val heapMembers:IndexedSeq[heap.HeapMember] = labelGraph.innerNodes.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))
    
    //Raise sourceInnerNode's to I
    heapMembers(source.index).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //For any node that is reachable from this node not yet visited (because it's key is still in the heap)
      for(successor <- topNode.successors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = heapMembers(successor.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          //todo this is the only place where labelGraph shows up in the loops, and the only place where it gets mutated. Replace it with an ArrayBuffer to avoid the weird extra data.
          //todo report to intellij
          val label = labelGraph.relax(support.semiring)(source,topNode,successor)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labelGraph
  }

  def allPairsShortestPaths[Node,Label,Key](labelDigraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key]):Digraph[Node,Label] = {

    for(source <- labelDigraph.innerNodes) {
      dijkstraSingleSource(labelDigraph,support)(source)
    }
    labelDigraph
  }

}

