package walend.scalax.semiring

import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import walend.scalax.heap.{HeapOrdering, FibonacciHeap, Heap}

/**
 * An implementation of the Dijkstra's algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v1
 */
object Dijkstra {

  /**
   * Dijkstra's algorithm.
   */
  def dijkstra [N:Manifest,Label,Key <: HeapKey[Label]](labelGraph:MutableGraph[N,LDiEdge])
                                 (innerSourceNode:labelGraph.NodeT)
                                 (semiring:Semiring[Label],heapKeyFactory:HeapKeyFactory[Label,Key],heapOrdering:HeapOrdering[Key,Key]):Graph[N,LDiEdge] = {

    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.NodeT] = new FibonacciHeap(heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.NodeT,heap.HeapMember] = labelGraph.nodes.map(node => (node -> heap.insert(heapKeyFactory.keyForLabel(semiring.O),node)))(breakOut)

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(innerSourceNode,throw new IllegalStateException("No HeapMember for innerSourceNode "+innerSourceNode)).raiseKey(heapKeyFactory.keyForLabel(semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //For any node that is reachable from this node not yet visited (because it's key is still in the heap)
      for(successor <- topNode.diSuccessors) {
        //if the node has not yet been visited (because it's key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(successor,throw new IllegalStateException("No HeapMember for "+successor))
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = semiring.relax(labelGraph)(innerSourceNode,topNode,successor)
          //Try to decrease the key
          heapKey.raiseKey(heapKeyFactory.keyForLabel(label))
        }
      }
    }

    labelGraph
  }

  def singleSourceShortestPaths[N:Manifest,Label,Key <: HeapKey[Label]](sourceNode:N,originalGraph:Graph[N,LDiEdge])
                                                 (semiring:Semiring[Label],heapKeyFactory:HeapKeyFactory[Label,Key],heapOrdering:HeapOrdering[Key,Key])
                                                 (labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    val innerSourceNode:labelGraph.NodeT = labelGraph get sourceNode
    dijkstra(labelGraph)(innerSourceNode)(semiring,heapKeyFactory,heapOrdering)
  }


  /**
   * This method runs Dijkstra's algorithm for all nodes.
   */
  def allPairsShortestPaths[N:Manifest,Label,Key <: HeapKey[Label]](originalGraph:Graph[N,LDiEdge])
                                             (semiring:Semiring[Label],heapKeyFactory:HeapKeyFactory[Label,Key],heapOrdering:HeapOrdering[Key,Key])
                                             (labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    for(node <- labelGraph.nodes) {
      dijkstra(labelGraph)(node)(semiring,heapKeyFactory,heapOrdering)
    }
    labelGraph
  }
}

