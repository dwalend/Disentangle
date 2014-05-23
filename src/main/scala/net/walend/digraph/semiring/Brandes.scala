package net.walend.digraph.semiring

import net.walend.heap.{FibonacciHeap, Heap}
import scala.collection.mutable.ArrayBuffer

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Brandes {

  def relaxSink[Node,Label,Key](digraph:IndexedDigraph[Node,Label],
                                  labels:ArrayBuffer[Label],
                                  semiring:SemiringSupport[Label,Key]#Semiring)
                                 (from:digraph.InnerNodeType,
                                  through:digraph.InnerNodeType,
                                  to:digraph.InnerNodeType):Label = {

    val fromThrough:Label = digraph.edge(from,through)
    val throughTo:Label = labels(through.index)
    val fromThroughTo:Label = semiring.extend(fromThrough,throughTo)

    val current:Label = labels(from.index)
    val summaryLabel:Label = semiring.summary(fromThroughTo,current)

    summaryLabel
  }

  /**
   * Dijkstra's algorithm for a single sink, with a Seq of visited edges.
   */
  def dijkstraSingleSink[Node,Label,Key](initialGraph:IndexedDigraph[Node,Label],support:SemiringSupport[Label,Key])
                                          (sink:initialGraph.InnerNodeType):(Seq[(Node,Node,Label)],
                                                                             Seq[(initialGraph.InnerNodeType,initialGraph.InnerNodeType,Label)]) = {
    //Set up the map of Nodes to HeapKeys
    val labels:ArrayBuffer[Label] = ArrayBuffer.fill(initialGraph.nodes.size)(support.semiring.O)

    val heap:Heap[Key,initialGraph.InnerNodeType] = new FibonacciHeap(support.heapOrdering)

    val heapMembers:IndexedSeq[heap.HeapMember] = initialGraph.innerNodes.map(node => heap.insert(support.heapKeyForLabel(support.semiring.O),node))

    //Stack of visited nodes
    import scala.collection.mutable.Stack
    //todo take in the heap type as a parameter. Then create a specialized heap to make this stack get recorded as a side effect of takeTopValue. Then make dijkstraSingleSink part of Dikstra
    val stack = Stack[(initialGraph.InnerNodeType,initialGraph.InnerNodeType,Label)]()

    //Raise sourceInnerNode's to I
    labels(sink.index) = support.semiring.I
    heapMembers(sink.index).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //todo make this a side effect of takeTopValue
      stack.push((topNode,sink,labels(topNode.index)))

      //For any node that can reach this node not yet visited (because it's key is still in the heap)
      for(predecessor <- topNode.predecessors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = heapMembers(predecessor.index)
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = relaxSink(initialGraph,labels,support.semiring)(predecessor,topNode,sink)
          labels(predecessor.index) = label
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    val finalLabels = labels.zipWithIndex.map(x => (initialGraph.node(x._2),sink.value,x._1)).filter(x => x._3 != support.semiring.O)
    (finalLabels,stack)
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
//todo can definitely just use Nodes instead of InnerNodeTypes in here
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[Node, Double] = MutableMap()

    //for each possible choice of next step
    for (edge <- edges) {
      //figure out the partial betweenness to apply to that step
      val label: Label = edge._3
      label match {
        case None => //nothing to do
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
                                  (initialGraph:IndexedDigraph[Node,Option[FirstSteps[Node, CoreLabel]]],support:AllPathsFirstSteps[Node, CoreLabel, Key]):(Seq[(Node,Node,Option[FirstSteps[Node,CoreLabel]])],Map[Node, Double]) = {

    type Label = support.Label

    val edgesAndPartialBetweennesses:Seq[(Seq[(Node,Node,Label)],Map[Node, Double])] = for(sink <- initialGraph.innerNodes) yield {
      val edgesAndNodeStack:(Seq[(Node,Node,Label)],
        Seq[(initialGraph.InnerNodeType,initialGraph.InnerNodeType,Label)]) = dijkstraSingleSink(initialGraph,support)(sink)
      val partialB = partialBetweenness(support,initialGraph)(sink,edgesAndNodeStack._2)
      (edgesAndNodeStack._1,partialB)
    }

    def betweennessForNode(node: Node): Double = {
      edgesAndPartialBetweennesses.map(x => x._2.getOrElse(node, 0.0)).sum
    }
    val betweennessMap:Map[Node, Double] = initialGraph.nodes.map(node => (node, betweennessForNode(node))).toMap

    val edges:Seq[(Node,Node,Label)] = edgesAndPartialBetweennesses.map(x => x._1).flatten

    (edges,betweennessMap)
  }
}