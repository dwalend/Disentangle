package walend.scalax.semiring

import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import walend.scalax.heap.{FibonacciHeap, Heap}

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
                                 (support:GraphMinimizerSupport[Label,Key]):Graph[N,LDiEdge] = {

    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.NodeT] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.NodeT,heap.HeapMember] = labelGraph.nodes.map(node => (node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node)))(breakOut)

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(innerSourceNode,throw new IllegalStateException("No HeapMember for innerSourceNode "+innerSourceNode)).raiseKey(support.heapKeyForLabel(support.semiring.I))

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
          val label = support.semiring.relax(labelGraph)(innerSourceNode,topNode,successor)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labelGraph
  }

  def singleSourceShortestPaths[N:Manifest,Label,Key <: HeapKey[Label]](sourceNode:N,originalGraph:Graph[N,LDiEdge])
                                                 (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(support.semiring)
    val innerSourceNode:labelGraph.NodeT = labelGraph get sourceNode
    dijkstra(labelGraph)(innerSourceNode)(support)
  }


  /**
   * This method runs Dijkstra's algorithm for all nodes.
   */
  def allPairsShortestPaths[N:Manifest,Label,Key <: HeapKey[Label]](originalGraph:Graph[N,LDiEdge])
                                             (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(support.semiring)
    for(node <- labelGraph.nodes) {
      dijkstra(labelGraph)(node)(support)
    }
    labelGraph
  }
}

