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
  def dijkstraSingleSource [Node,Label,Key](labelGraph:Digraph[Node,Label],support:GraphMinimizerSupport[Label,Key])
                                        (sourceInnerNode:labelGraph.InnerNodeType):Digraph[Node,Label] = {
    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.InnerNodeType,heap.HeapMember] =
          labelGraph.innerNodes.map(node => node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node))(breakOut)

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(sourceInnerNode,throw new IllegalStateException("No HeapMember for sourceInnerNode "+sourceInnerNode)).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //For any node that is reachable from this node not yet visited (because it's key is still in the heap)
      for(successor <- topNode.successors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(successor,throw new IllegalStateException("No HeapMember for "+successor))
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = support.semiring.relax(labelGraph)(sourceInnerNode,topNode,successor)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labelGraph
  }
  /*
  def singleSourceShortestPaths[Node,Label,Key]
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
  */

  def allPairsShortestPaths[Node,Edge,Label,Key](digraph:Digraph[Node,Edge],graphConverter:Digraph[Node,Edge]=>Digraph[Node,Label],support:GraphMinimizerSupport[Label,Key]):Digraph[Node,Label] = {

    val labelDigraph = graphConverter(digraph)

    for(source <- labelDigraph.innerNodes) {
      dijkstraSingleSource(labelDigraph,support)(source)
    }
    labelDigraph
  }

}

