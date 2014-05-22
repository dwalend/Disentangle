package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.{FibonacciHeap, Heap}
import scala.collection.mutable.ArrayBuffer

/**
 * An implementation of Dijkstra's algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Dijkstra {


  def relaxSource[Node,Label,Key](digraph:IndexedDigraph[Node,Label],labels:ArrayBuffer[Label],semiring:SemiringSupport[Label,Key]#Semiring)
                (from:digraph.InnerNodeType,
                 through:digraph.InnerNodeType,
                 to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = labels(through.index)
    val throughTo:Label = digraph.edge(through,to)
    val fromThroughTo:Label = semiring.extend(fromThrough,throughTo)

    val current:Label = labels(to.index)
    val summaryLabel:Label = semiring.summary(fromThroughTo,current)

    summaryLabel
  }

  /**
   * Dijkstra's algorithm.
   */
  def dijkstraSingleSource[Node,Label,Key](initialGraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key])
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

}

