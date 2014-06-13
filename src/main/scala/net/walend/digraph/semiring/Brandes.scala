package net.walend.digraph.semiring

import net.walend.heap.{FibonacciHeap, Heap}
import net.walend.digraph.IndexedLabelDigraph
import scala.collection.GenTraversable

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Brandes {
  import scala.collection.mutable.Stack
  /**
   * Dijkstra's algorithm for a single sink, with a Seq of visited arcs to support Brandes' algorithm.
   *
   * O(n ln(n) + a)
   */
  def dijkstraSingleSinkForBrandes[Node,Label,Key](labelDigraph:IndexedLabelDigraph[Node,Label],
                                         support:SemiringSupport[Label,Key])
                                        (sink:labelDigraph.InnerNodeType):(IndexedSeq[(Node,Node,Label)],
                                                                            Stack[(labelDigraph.InnerNodeType,Label)]) = {
    val stack = Stack[labelDigraph.InnerNodeType]()

    val heap:Heap[Key,labelDigraph.InnerNodeType] = new FibonacciHeap[Key,labelDigraph.InnerNodeType](support.heapOrdering) {

      override def takeTopValue():labelDigraph.InnerNodeType = {
        val result = super.takeTopValue()
        stack.push(result)
        result
      }
    }

    val allArcs = Dijkstra.dijkstraSingleSinkCustomHeap(labelDigraph,support)(sink,heap)
    val originArcStack:Stack[(labelDigraph.InnerNodeType,Label)] = stack.map(x => (x,allArcs(x.index)._3))

    (allArcs,originArcStack.filter(_._2 != support.semiring.O))
  }

    /**
   * Find partial betweenness
     *
     * O(a ln(n)) but could be O(a)
   */
  def partialBetweenness[Node, 
                          CoreLabel, 
                          Label <: Option[FirstSteps[Node, CoreLabel]], 
                          Key]
                          (support: AllPathsFirstSteps[Node, CoreLabel, Key],labelGraph:IndexedLabelDigraph[Node,Label])
                          (sink: labelGraph.InnerNodeType,stack:Stack[(labelGraph.InnerNodeType,Label)],shortestPathsToSink:IndexedSeq[(Node,Node,Label)]): Map[Node, Double] = {
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[Node, Double] = MutableMap()

    //for each possible choice of next step in the stack
    while(!stack.isEmpty) {
      val arc = stack.pop() //w
      //figure out the partial betweenness to apply to that step
      val label: Label = arc._2
      label match {
        case None => //nothing to do
        case Some(sourceLabel: FirstSteps[Node, CoreLabel]) => {
          val sourceCount: Double = sourceLabel.pathCount
          val partialFromSource:Double = nodesToPartialBetweenness.getOrElse(arc._1.value, 0.0)
          for (choice <- sourceLabel.choices) {
            //only calculate betweenness for the between nodes, not arriving at the sink
            if (choice != sink.value)  {
              val oldPartial: Double = nodesToPartialBetweenness.getOrElse(choice, 0.0)
              //todo switch over the choice to be an innerNode to avoid the index lookup and O(ln(n)) cost
              val choiceIndex: Int = labelGraph.innerNode(choice).get.index
              val choiceLabel:Label = shortestPathsToSink(choiceIndex)._3
              if(choiceLabel != None) {
                val choiceCount: Double = choiceLabel.get.pathCount
                //new value is the old value plus (value for coming through the source, plus the source)/number of choices
                val newPartial: Double = oldPartial + ((1.0 + partialFromSource) * (choiceCount / sourceCount))
                nodesToPartialBetweenness.put(choice, newPartial)
              }
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
                                  (initialGraph:IndexedLabelDigraph[Node,Option[FirstSteps[Node, CoreLabel]]],support:AllPathsFirstSteps[Node, CoreLabel, Key]):(Seq[(Node,Node,Option[FirstSteps[Node,CoreLabel]])],Map[Node, Double]) = {

    type Label = support.Label

    val arcsAndPartialBetweennesses:Seq[(Seq[(Node,Node,Label)],Map[Node, Double])] = for(sink <- initialGraph.innerNodes) yield {
      val arcsAndNodeStack:(IndexedSeq[(Node,Node,Label)],Stack[(initialGraph.InnerNodeType,Label)]) = dijkstraSingleSinkForBrandes(initialGraph,support)(sink)
      val partialB = partialBetweenness(support,initialGraph)(sink,arcsAndNodeStack._2,arcsAndNodeStack._1)
      (arcsAndNodeStack._1.filter(_._3 != support.semiring.O),partialB)
    }

    def betweennessForNode(node: Node): Double = {
      arcsAndPartialBetweennesses.map(x => x._2.getOrElse(node, 0.0)).sum
    }
    val betweennessMap:Map[Node, Double] = initialGraph.nodes.map(node => (node, betweennessForNode(node))).toMap

    val arcs:Seq[(Node,Node,Label)] = arcsAndPartialBetweennesses.map(x => x._1).flatten

    (arcs,betweennessMap)
  }

  /**
   * Create a digraph of Labels from an arc list.
   *
   * @return an IndexedDigraph with graph's nodes, a self-arc for each node with the semiring's identifier, and an arc for each arc specified by labelForArc.
   */
  def createLabelDigraph[Node,ArcLabel,CoreLabel,Key](arcs:GenTraversable[(Node,Node,ArcLabel)] = Seq.empty,
                                                  extraNodes:Seq[Node] = Seq.empty,
                                                  support:AllPathsFirstSteps[Node, CoreLabel, Key],
                                                  labelForArc:(Node,Node,ArcLabel)=>CoreLabel):IndexedLabelDigraph[Node,Option[FirstSteps[Node,CoreLabel]]] = {

    Dijkstra.createLabelDigraph(arcs,extraNodes,support,support.convertArcToLabelFunc[ArcLabel](labelForArc))
  }

  def allLeastPathsAndBetweenness[Node,ArcLabel,CoreLabel,Key](arcs:GenTraversable[(Node,Node,ArcLabel)] = Seq.empty,
                                                           extraNodes:Seq[Node] = Seq.empty,
                                                           support:AllPathsFirstSteps[Node, CoreLabel, Key],
                                                           labelForArc:(Node,Node,ArcLabel)=>CoreLabel):(Seq[(Node,Node,Option[FirstSteps[Node,CoreLabel]])],Map[Node, Double]) = {
    val labelGraph =  Dijkstra.createLabelDigraph(arcs,extraNodes,support,support.convertArcToLabelFunc[ArcLabel](labelForArc))
    allLeastPathsAndBetweenness(labelGraph,support)
  }
}