package net.walend.digraph.semiring

import net.walend.heap.{FibonacciHeap, Heap}
import scala.collection.mutable.ArrayBuffer
import net.walend.digraph.{IndexedDigraph, Digraph}

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Brandes {

  /**
   * Dijkstra's algorithm for a single sink, with a Seq of visited edges to support Brandes' algorithm.
   */
  def dijkstraSingleSinkForBrandes[Node,Label,Key](initialDigraph:IndexedDigraph[Node,Label],
                                         support:SemiringSupport[Label,Key])
                                        (sink:initialDigraph.InnerNodeType):(Seq[(Node,Node,Label)],
                                                                            Seq[(Node,Label)]) = {
    import scala.collection.mutable.Stack
    val stack = Stack[initialDigraph.InnerNodeType]()

    val heap:Heap[Key,initialDigraph.InnerNodeType] = new FibonacciHeap[Key,initialDigraph.InnerNodeType](support.heapOrdering) {

      override def takeTopValue():initialDigraph.InnerNodeType = {
        val result = super.takeTopValue()
        stack.push(result)
        result
      }
    }

    val allEdges = Dijkstra.dijkstraSingleSinkCustomHeap(initialDigraph,support)(sink,heap)
    val originEdgeStack = stack.map(x => (x.value,allEdges(x.index)._3))

    //todo filter allEdges
    (allEdges.filter(x => x._3 != support.semiring.O),originEdgeStack)
  }

    /**
   * Find partial betweenness
   */
  def partialBetweenness[Node, 
                          CoreLabel, 
                          Label <: Option[FirstSteps[Node, CoreLabel]], 
                          Key]
                          (support: AllPathsFirstSteps[Node, CoreLabel, Key],labelGraph:Digraph[Node,Label])
                          (sink: labelGraph.InnerNodeType,stack:Seq[(Node,Label)]): Map[Node, Double] = {
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[Node, Double] = MutableMap()

    //for each possible choice of next step
    for (edge <- stack) {
      //figure out the partial betweenness to apply to that step
      val label: Label = edge._2
      label match {
        case None => //nothing to do
        case Some(sourceLabel: FirstSteps[Node, CoreLabel]) => {
          val numChoices: Double = sourceLabel.choices.size
          val partialFromSource:Double = nodesToPartialBetweenness.getOrElse(edge._1, 0.0)
          for (choice <- sourceLabel.choices) {
            //only calculate betweenness for the between nodes, not arriving at the sink
            if (choice != sink.value)  {
              val oldPartial: Double = nodesToPartialBetweenness.getOrElse(choice, 0.0)
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
      val edgesAndNodeStack:(Seq[(Node,Node,Label)],Seq[(Node,Label)]) = dijkstraSingleSinkForBrandes(initialGraph,support)(sink)
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