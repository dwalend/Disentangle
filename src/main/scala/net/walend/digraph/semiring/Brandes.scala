package net.walend.digraph.semiring

import net.walend.heap.{FibonacciHeap, Heap}
import net.walend.digraph.{IndexedLabelDigraph, LabelDigraph}

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */

object Brandes {

  /**
   * Dijkstra's algorithm for a single sink, with a Seq of visited arcs to support Brandes' algorithm.
   */
  def dijkstraSingleSinkForBrandes[Node,Label,Key](initialDigraph:IndexedLabelDigraph[Node,Label],
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

    val allArcs = Dijkstra.dijkstraSingleSinkCustomHeap(initialDigraph,support)(sink,heap)
    val originArcStack = stack.map(x => (x.value,allArcs(x.index)._3))

    //todo filter allArcs
    (allArcs.filter(x => x._3 != support.semiring.O),originArcStack)
  }

    /**
   * Find partial betweenness
   */
  def partialBetweenness[Node, 
                          CoreLabel, 
                          Label <: Option[FirstSteps[Node, CoreLabel]], 
                          Key]
                          (support: AllPathsFirstSteps[Node, CoreLabel, Key],labelGraph:LabelDigraph[Node,Label])
                          (sink: labelGraph.InnerNodeType,stack:Seq[(Node,Label)]): Map[Node, Double] = {
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[Node, Double] = MutableMap()

    //for each possible choice of next step
    for (arc <- stack) {
      //figure out the partial betweenness to apply to that step
      val label: Label = arc._2
      label match {
        case None => //nothing to do
        case Some(sourceLabel: FirstSteps[Node, CoreLabel]) => {
          val numChoices: Double = sourceLabel.choices.size
          val partialFromSource:Double = nodesToPartialBetweenness.getOrElse(arc._1, 0.0)
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
                                  (initialGraph:IndexedLabelDigraph[Node,Option[FirstSteps[Node, CoreLabel]]],support:AllPathsFirstSteps[Node, CoreLabel, Key]):(Seq[(Node,Node,Option[FirstSteps[Node,CoreLabel]])],Map[Node, Double]) = {

    type Label = support.Label

    val arcsAndPartialBetweennesses:Seq[(Seq[(Node,Node,Label)],Map[Node, Double])] = for(sink <- initialGraph.innerNodes) yield {
      val arcsAndNodeStack:(Seq[(Node,Node,Label)],Seq[(Node,Label)]) = dijkstraSingleSinkForBrandes(initialGraph,support)(sink)
      val partialB = partialBetweenness(support,initialGraph)(sink,arcsAndNodeStack._2)
      (arcsAndNodeStack._1,partialB)
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
  def createLabelDigraph[Node,ArcLabel,CoreLabel,Key](arcs:Seq[(Node,Node,ArcLabel)] = Seq.empty,
                                                  extraNodes:Seq[Node] = Seq.empty,
                                                  support:AllPathsFirstSteps[Node, CoreLabel, Key],
                                                  labelForArc:(Node,Node,ArcLabel)=>CoreLabel):IndexedLabelDigraph[Node,Option[FirstSteps[Node,CoreLabel]]] = {

    Dijkstra.createLabelDigraph(arcs,extraNodes,support,support.convertArcToLabelFunc[ArcLabel](labelForArc))
  }

  def allLeastPathsAndBetweenness[Node,ArcLabel,CoreLabel,Key](arcs:Seq[(Node,Node,ArcLabel)] = Seq.empty,
                                                           extraNodes:Seq[Node] = Seq.empty,
                                                           support:AllPathsFirstSteps[Node, CoreLabel, Key],
                                                           labelForArc:(Node,Node,ArcLabel)=>CoreLabel):(Seq[(Node,Node,Option[FirstSteps[Node,CoreLabel]])],Map[Node, Double]) = {
    val labelGraph =  Dijkstra.createLabelDigraph(arcs,extraNodes,support,support.convertArcToLabelFunc[ArcLabel](labelForArc))
    allLeastPathsAndBetweenness(labelGraph,support)
  }
}