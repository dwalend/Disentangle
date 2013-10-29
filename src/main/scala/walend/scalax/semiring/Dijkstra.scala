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

  /* todo later
  def singleSourceShortestPaths[N:Manifest,Label](originalGraph:Graph[N,LDiEdge])
                                                 (sourceNode:originalGraph.NodeT)
                                                 (semiring:Semiring[Label])
                                                 (labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {
    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
  */
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

