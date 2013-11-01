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

  def dijkstra[N,Label](labelGraph:MutableGraph[N,LDiEdge])
                       (sourceNode:labelGraph.NodeT)
                       (semiring:Semiring[Label],heapOrdering:HeapOrdering[Label]):Unit = {

    val heap:Heap[Label,labelGraph.EdgeT] = new FibonacciHeap[Label,labelGraph.EdgeT](heapOrdering)


  }

  def singleSourceShortestPaths[N:Manifest,Label](sourceNode:N,originalGraph:Graph[N,LDiEdge])
                                                 (semiring:Semiring[Label],heapKeyFactory:HeapKeyFactory[Label],heapOrdering:HeapOrdering[HeapKey[Label]])
                                                 (labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {
    //setup

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    val innerSourceNode:labelGraph.NodeT = labelGraph get sourceNode


    //Dijkstra's algorithm

    //Set up the map of Nodes to HeapKeys
    val heap:Heap[HeapKey[Label],labelGraph.NodeT] = new FibonacciHeap(heapOrdering)

    import scala.collection.mutable.{Map => MutableMap,Set => MutableSet}
    //todo this map doesn't have to be mutable
    val nodesToHeapMembers:MutableMap[labelGraph.NodeT,heap.HeapMember] = MutableMap()

    //Set up the heap. All keys are O
    for(node <- labelGraph.nodes) {
      val heapMember = heap.insert(heapKeyFactory.keyForLabel(semiring.O),node)
      nodesToHeapMembers.put(node,heapMember)
    }

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

  def floydWarshall[N,Label](labelGraph:MutableGraph[N,LDiEdge])(semiring:Semiring[Label]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }



  def allPairsShortestPaths[N:Manifest,Label](originalGraph:Graph[N,LDiEdge])(semiring:Semiring[Label])(labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {
    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)

    for(node <- labelGraph.nodes) {
//todo make a HeapOrdering out of the Semiring's annihilator and summary operator      dijkstra(labelGraph)(node)(semiring)
    }

    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
}

