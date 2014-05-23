package net.walend.scalagraph.minimizer.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import net.walend.heap.{FibonacciHeap, Heap}
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * An implementation of Dijkstra's algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.0.0
 */
object Dijkstra {

  /**
   * Dijkstra's algorithm.
   */
  def dijkstraSingleSource [N,Label,Key](labelGraph:MutableGraph[N,MLDiEdge])
                                             (innerSourceNode:labelGraph.NodeT)
                                             (support:GraphMinimizerSupport[Label,Key]):Graph[N,MLDiEdge] = {
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
        //if the node has not yet been visited (because its key is still in the heap)
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

  def singleSourceShortestPaths[N,Label,Key]
                              (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:AbsractLabelGraphBuilder[N,Label])
                              (sourceNode:N,originalGraph:Graph[N,MLDiEdge]):Graph[N,MLDiEdge] = {

    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)
    val innerSourceNode:labelGraph.NodeT = labelGraph get sourceNode
    dijkstraSingleSource(labelGraph)(innerSourceNode)(support)
  }


  import scala.language.higherKinds

  /**
   * This method runs Dijkstra's algorithm for all nodes in the label graph.
   */
  def allPairsShortestPaths[N,
                            Label,
                            Key]
                            (support:GraphMinimizerSupport[Label,Key])
                            (labelGraph:MutableGraph[N,MLDiEdge]):Graph[N,MLDiEdge] = {

    for(node <- labelGraph.nodes) {
      dijkstraSingleSource(labelGraph)(node)(support)
    }
    labelGraph
  }


  /**
   * This method creates the label graph and then runs Dijkstra's algorithm for all nodes.
   */
  def allPairsShortestPaths[N,
                            E[X] <: EdgeLikeIn[X],
                            Label,
                            Key]
                            (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[N,Label])
                            (originalGraph:Graph[N,E]):Graph[N,MLDiEdge] = {

    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)
    allPairsShortestPaths(support)(labelGraph)

    labelGraph
  }
}

