package net.walend.scalagraph.minimizer.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import net.walend.heap.{HeapOrdering, FibonacciHeap, Heap}

/**
 * An implementation of Prim's algorithm for minimum spanning trees
 *
 * @author dwalend
 * @since v0.0.0
 */
object Prim {

  /**
   * Prim's algorithm.
   *
   * @return A Set of Sets of Edges that make up minimum trees, and a Set of isolates. If there's one Set of edges and no isolates then you've got a minimum spanning tree
   */
  //todo labelGraph is just a graph
  /*
  def prim[N:Manifest,Label,Key](labelGraph:Graph[N,LDiEdge])
                                (support:GraphMinimizerSupport[Label,Key]):(Set[Set[LDiEdge[N]]],Set[N]) = {

    //Set up the map of Edges to HeapKeys
    val heap:Heap[Key,labelGraph.EdgeT] = new FibonacciHeap(support.heapOrdering)
    val annihilatorKey = support.heapKeyForLabel(support.semiring.O)

    import scala.collection.breakOut
    val edgesToHeapMembers:Map[labelGraph.EdgeT,heap.HeapMember] = labelGraph.edges.map(edge => (edge -> heap.insert(annihilatorKey,edge)))(breakOut)

    //Create a mutable map with all nodes
    import scala.collection.{Set => MutableSet}
    val nodesNotIncluded:MutableSet[labelGraph.NodeT] = labelGraph.nodes.to[MutableSet]

    var trees:MutableSet[MutableSet[labelGraph.EdgeT]] = MutableSet[MutableSet[labelGraph.EdgeT]]()
    //while the heap is not empty and some nodes have not been visited
    var currentTree:MutableSet[labelGraph.EdgeT] = null
    while((!heap.isEmpty)&&(!nodesNotIncluded.isEmpty)) {

      //take the top edge
      val topMember:heap.HeapMember = heap.takeTop()
      currentTree = if(topMember.key==annihilatorKey) {
        if(currentTree!=null) {
          trees = trees + currentTree
        }
        Set[LDiEdge[N]]()
      }
      else currentTree


      //if it's a new tree -- heap key = O -- then start a new set of edges
      //

      //raise the seed node's edges to their values
      for(edge <- seed.edges) {
        //todo handle better
        val heapKey = edgesToHeapMembers.getOrElse(edge, throw new RuntimeException)
        heapKey.raiseKey(support.heapKeyForLabel(edge.label))
      }

      //figure out which end (or if both) are the new endpoint


      //for the node that has not yet been visited

      //Add the node and edge to the MST, and remove the node from the set of all nodes to be visited

      //For each successor and predecessor edge still in the heap

      //Raise the heap key for that edge to label


    }




    (,nodesNotIncluded)
  }


  def prim[N:Manifest,Label,Key](labelGraph:MutableGraph[N,LDiEdge])
                                     (innerSourceNode:labelGraph.NodeT)
                                     (support:GraphMinimizerSupport[Label,Key]):Graph[N,LDiEdge] = {


    //todo start with a set of edges from initialEdgeFromGraphEdge, not labelGraph.
    //Set up the map of Nodes to HeapKeys
    //todo figure out how to track edges and add edges to the label graph
    val heap:Heap[Key,labelGraph.NodeT] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.NodeT,heap.HeapMember] = labelGraph.nodes.map(node => (node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node)))(breakOut)

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(innerSourceNode,throw new IllegalStateException("No HeapMember for innerSourceNode "+innerSourceNode)).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {

      //take the top node
      val topNode = heap.takeTopValue()
      //For any node that is reachable from this node not yet visited (because its key is still in the heap)
      //todo get the edges instead
      for(successor <- topNode.diSuccessors) {
        //if the node has not yet been visited (because it's key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(successor,throw new IllegalStateException("No HeapMember for "+successor))
        if(heapKey.isInHeap) {

//todo convert the current key into a label
          //todo get the label from the edge
          //todo use the summary operator to get the least label
          val label = support.semiring.summary()
          //todo raise the heapKey to the heapKey for the new label

      /*
          for each node reachable from it not currently in the MST
          if some node is closer by using this node
            replace the key with the closer value
      */




          //Relax to get a new label
          val label = support.semiring.relax(labelGraph)(innerSourceNode,topNode,successor)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    labelGraph
  }
  */
}

