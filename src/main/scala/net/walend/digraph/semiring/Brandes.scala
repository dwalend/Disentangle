package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.{FibonacciHeap, Heap}

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Brandes {

  /**
   * Dijkstra's algorithm, run backwards, with a Seq of the visited edges.
   */
  //todo specify Label, and use a different support class
  def dijkstraSingleSink [Node,
                          CoreLabel,
                          Key]
                          (labelGraph:Digraph[Node,Option[FirstSteps[Node, CoreLabel]]],support:AllPathsFirstSteps[Node, CoreLabel, Key])
                          (sink:labelGraph.InnerNodeType):(Digraph[Node,Option[FirstSteps[Node, CoreLabel]]],Seq[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Option[FirstSteps[Node, CoreLabel]])]) = {
    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.InnerNodeType,heap.HeapMember] =
      labelGraph.innerNodes.map(node => node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node))(breakOut)

    //Raise sinkInnerNode's to I
    nodesToHeapMembers.getOrElse(sink,throw new IllegalStateException("No HeapMember for sinkInnerNode "+sink)).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //Stack of visited nodes
    import scala.collection.mutable.Stack
    //todo take in the heap type as a parameter. Then create a specialized heap to make this stack get recorded as a side effect of takeTopValue. Then make dijkstraSingleSink part of Dikstra
    val stack = Stack[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Option[FirstSteps[Node, CoreLabel]])]()

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      labelGraph.edge(topNode,sink) match {
        case None => //support.semiring.O => ;//ignore
        case label:Some[FirstSteps[Node, CoreLabel]] => stack.push((topNode,sink,label))
      }

      //For any node that this node can reach and is not yet visited (because it's key is still in the heap)
      for(predecessor <- topNode.predecessors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(predecessor,throw new IllegalStateException("No HeapMember for "+predecessor))
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = support.semiring.relax[Node](labelGraph)(predecessor,topNode,sink)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    (labelGraph,stack)
  }

  /**
   * Find partial betweenness
   */
  def partialBetweenness[Node, 
                          CoreLabel, 
                          Label <: Option[FirstSteps[Node, CoreLabel]], 
                          Key]
                          (support: AllPathsFirstSteps[Node, CoreLabel, Key],labelGraph:Digraph[Node,Label])
                          (sink: labelGraph.InnerNodeType,edges:Seq[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)]): Map[Node, Double] = {

    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[Node, Double] = MutableMap()

    //for each possible choice of next step
    for (edge <- edges) {
      //figure out the partial betweenness to apply to that step
      val label: Label = edge._3
      label match {
        case None => //should never happen, but do nothing if it does
        case Some(sourceLabel: FirstSteps[Node, CoreLabel]) => {
          val numChoices: Double = sourceLabel.choices.size
          val partialFromSource = nodesToPartialBetweenness.getOrElse(edge._1.value, 0.0)
          for (choice <- sourceLabel.choices) {
            //only calculate betweenness for the between nodes, not arriving at the sink
            if (choice != sink.value)  {
              val oldPartial: Double = nodesToPartialBetweenness.getOrElse(choice, 0)
              //new value is the old value plus (value for coming through the source, plus the source)/number of choices
              val newPartial: Double = oldPartial + ((1.0 + partialFromSource) / numChoices)
              nodesToPartialBetweenness.put(choice, newPartial)
            }
          }
        }
      }
    }

    nodesToPartialBetweenness.toMap
  }


  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def allLeastPathsAndBetweenness[Node,
                                  CoreLabel,
                                  Key]
                                  (labelGraph:Digraph[Node,Option[FirstSteps[Node, CoreLabel]]],support:AllPathsFirstSteps[Node, CoreLabel, Key]):(Digraph[Node,Option[FirstSteps[Node, CoreLabel]]],Map[Node, Double]) = {

    val partialBetweennesses:Seq[Map[Node, Double]] = for(sink <- labelGraph.innerNodes) yield {
      val labelGraphAndNodeStack = dijkstraSingleSink[Node,CoreLabel,Key](labelGraph,support)(sink)
      partialBetweenness(support,labelGraph)(sink,labelGraphAndNodeStack._2)
    }

    def betweennessForNode(node: Node): Double = {
      partialBetweennesses.map(x => x.getOrElse(node, 0.0)).sum
    }
    val betweennessMap:Map[Node, Double] = labelGraph.nodes.map(node => (node, betweennessForNode(node))).toMap

    (labelGraph,betweennessMap)
  }
}