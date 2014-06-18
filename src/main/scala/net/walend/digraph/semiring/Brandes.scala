package net.walend.digraph.semiring

import net.walend.heap.{HeapOrdering, FibonacciHeap, Heap}
import net.walend.digraph.{AdjacencyLabelDigraph, IndexedLabelDigraph}
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
  def dijkstraSingleSinkForBrandes[Node, Label, Key](labelDigraph: IndexedLabelDigraph[Node, Label],
                                                     support: SemiringSupport[Label, Key])
                                                    (sink: labelDigraph.InnerNodeType): (IndexedSeq[(Node, Node, Label)],
    Stack[(labelDigraph.InnerNodeType, Label)]) = {
    val stack = Stack[labelDigraph.InnerNodeType]()

    val heap: Heap[Key, labelDigraph.InnerNodeType] = new FibonacciHeap[Key, labelDigraph.InnerNodeType](support.heapOrdering) {

      override def takeTopValue(): labelDigraph.InnerNodeType = {
        val result = super.takeTopValue()
        stack.push(result)
        result
      }
    }

    val allArcs = Dijkstra.dijkstraSingleSinkCustomHeap(labelDigraph, support)(sink, heap)
    val originArcStack: Stack[(labelDigraph.InnerNodeType, Label)] = stack.map(x => (x, allArcs(x.index)._3))

    (allArcs, originArcStack.filter(_._2 != support.semiring.O))
  }

  /**
   * Find partial betweenness
   *
   * O(a)
   */
  def partialBetweenness[Node,
  CoreLabel,
  Label <: Option[BrandesSteps[Node, CoreLabel]],
  Key]
  (support: BrandesSupport[Node, CoreLabel, Key], labelGraph: IndexedLabelDigraph[Node, Label])
  (sink: labelGraph.InnerNodeType, stack: Stack[(labelGraph.InnerNodeType, Label)], shortestPathsToSink: IndexedSeq[(Node, Node, Label)]): IndexedSeq[Double] = {
    import scala.collection.mutable.ArrayBuffer
    val partialBetweenness: ArrayBuffer[Double] = ArrayBuffer.fill(labelGraph.nodes.size)(0.0)

    //for each possible choice of next step in the stack
    while (!stack.isEmpty) {
      val arc = stack.pop() //w
      //figure out the partial betweenness to apply to that step
      val label: Label = arc._2
      label match {
        case None => //nothing to do
        case Some(sourceLabel: BrandesSteps[Node, CoreLabel]) => {
          val sourceCount: Double = sourceLabel.pathCount
          val partialFromSource: Double = partialBetweenness(arc._1.index)
          for (choiceIndex <- sourceLabel.choiceIndexes) {
            //only calculate betweenness for the between nodes, not arriving at the sink
            if (choiceIndex != sink.index) {
              val oldPartial: Double = partialBetweenness(choiceIndex)
              val choiceLabel: Label = shortestPathsToSink(choiceIndex)._3
              if (choiceLabel != None) {
                val choiceCount: Double = choiceLabel.get.pathCount
                //new value is the old value plus (value for coming through the source, plus the source)/number of choices
                val newPartial: Double = oldPartial + ((1.0 + partialFromSource) * (choiceCount / sourceCount))
                partialBetweenness(choiceIndex) = newPartial
              }
            }
          }
        }
      }
    }

    partialBetweenness
  }


  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def allLeastPathsAndBetweenness[Node,
  CoreLabel,
  Key]
  (initialGraph: IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]], support: BrandesSupport[Node, CoreLabel, Key]): (Seq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {

    type Label = support.Label

    val arcsAndPartialBetweennesses: Seq[(Seq[(Node, Node, Label)], IndexedSeq[Double])] = for (sink <- initialGraph.innerNodes) yield {
      val arcsAndNodeStack: (IndexedSeq[(Node, Node, Label)], Stack[(initialGraph.InnerNodeType, Label)]) = dijkstraSingleSinkForBrandes(initialGraph, support)(sink)
      val partialB = partialBetweenness(support, initialGraph)(sink, arcsAndNodeStack._2, arcsAndNodeStack._1)
      (arcsAndNodeStack._1.filter(_._3 != support.semiring.O), partialB)
    }

    def betweennessForNode(innerNode: initialGraph.InnerNodeType): Double = {
      arcsAndPartialBetweennesses.map(x => x._2(innerNode.index)).sum
    }
    val betweennessMap: Map[Node, Double] = initialGraph.innerNodes.map(innerNode => (innerNode.value, betweennessForNode(innerNode))).toMap

    val arcs: Seq[(Node, Node, Label)] = arcsAndPartialBetweennesses.map(x => x._1).flatten

    (arcs, betweennessMap)
  }

  /**
   * Create a digraph of Labels from an arc list.
   *
   * @return an IndexedDigraph with graph's nodes, a self-arc for each node with the semiring's identifier, and an arc for each arc specified by labelForArc.
   */
  def createLabelDigraph[Node, ArcLabel, CoreLabel, Key](arcs: GenTraversable[(Node, Node, ArcLabel)] = Seq.empty,
                                                         extraNodes: Seq[Node] = Seq.empty,
                                                         support: BrandesSupport[Node, CoreLabel, Key],
                                                         labelForArc: (Node, Node, ArcLabel) => CoreLabel): IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]] = {

    //todo start here

    //Create the core label digraph to get everything's index
    val coreLabelDigraph:AdjacencyLabelDigraph[Node,CoreLabel] = Dijkstra.createLabelDigraph[Node,ArcLabel,CoreLabel,Key](arcs, extraNodes, support.coreSupport, labelForArc)

    //Use that to create the Brandes labels
    val brandesArcs = coreLabelDigraph.innerEdges.map(x => (x._1.value,x._2.value,support.convertCoreLabelToLabel(coreLabelDigraph)(x)))

    AdjacencyLabelDigraph(brandesArcs,coreLabelDigraph.nodes,support.semiring.O)
  }

  def allLeastPathsAndBetweenness[Node, ArcLabel, CoreLabel, Key](arcs: GenTraversable[(Node, Node, ArcLabel)] = Seq.empty,
                                                                  extraNodes: Seq[Node] = Seq.empty,
                                                                  support: BrandesSupport[Node, CoreLabel, Key],
                                                                  labelForArc: (Node, Node, ArcLabel) => CoreLabel): (Seq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {
    val labelGraph = createLabelDigraph(arcs, extraNodes, support, labelForArc)
    allLeastPathsAndBetweenness(labelGraph, support)
  }

  //todo remove choices
  case class BrandesSteps[Node, CoreLabel](weight: CoreLabel, pathCount: Int, choices: Set[Node], choiceIndexes:Set[Int]) {

    /**
     * Overriding equals to speed up.
     */
    //todo pathCount
    override def equals(any: Any) = {
      if (any.isInstanceOf[BrandesSteps[Node, CoreLabel]]) {
        val other: BrandesSteps[Node, CoreLabel] = any.asInstanceOf[BrandesSteps[Node, CoreLabel]]
        if (this eq other) true //if they share a memory address, no need to compare
        else {
          if (weight == other.weight) {
            choices == other.choices
          } else false
        }
      } else false
    }

    /**
     * Overriding hashCode because I overrode equals.
     */
    //todo pathCount
    override def hashCode(): Int = {
      weight.hashCode() ^ choices.hashCode()
    }
  }

  class BrandesSupport[Node, CoreLabel, Key](val coreSupport: SemiringSupport[CoreLabel, Key]) extends SemiringSupport[Option[BrandesSteps[Node, CoreLabel]], Key] {

    override type Label = Option[BrandesSteps[Node, CoreLabel]]

    def semiring: Semiring = BrandesSemiring

    def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

    def heapKeyForLabel = {
      case Some(nextStep) => coreSupport.heapKeyForLabel(nextStep.weight)
      case None => coreSupport.heapOrdering.AlwaysBottom
    }

    def convertCoreLabelToLabel(labelDigraph:AdjacencyLabelDigraph[Node,CoreLabel])
                         (arc:labelDigraph.InnerEdgeType): Label = {
      Some(BrandesSteps[Node, CoreLabel](arc._3, 1, Set(arc._2.value), Set(arc._2.index)))
    }

    /**
     * A semiring for use with Brandes algorithm
     */
    object BrandesSemiring extends Semiring {

      def inDomain(label: Label): Boolean = {
        label match {
          case Some(steps: BrandesSteps[Node, CoreLabel]) => coreSupport.semiring.inDomain(steps.weight)
          case None => true
        }
      }

      //identity and annihilator
      val I = Some(BrandesSteps[Node, CoreLabel](coreSupport.semiring.I, 1, Set.empty, Set.empty))
      val O = None

      def summary(fromThroughToLabel: Label, currentLabel: Label): Label = {

        if (currentLabel != O) {
          if (fromThroughToLabel != O) {
            val currentSteps: BrandesSteps[Node, CoreLabel] = currentLabel.get
            val fromThroughToSteps: BrandesSteps[Node, CoreLabel] = fromThroughToLabel.get
            val summ = coreSupport.semiring.summary(fromThroughToSteps.weight, currentSteps.weight)
            if ((summ == fromThroughToSteps.weight) && (summ == currentSteps.weight)) {
              Some(new BrandesSteps[Node, CoreLabel](currentSteps.weight,
                currentSteps.pathCount + fromThroughToSteps.pathCount,
                currentSteps.choices ++ fromThroughToSteps.choices,
                currentSteps.choiceIndexes ++ fromThroughToSteps.choiceIndexes))
            }
            else if (summ == fromThroughToSteps.weight) fromThroughToLabel
            else if (summ == currentSteps.weight) currentLabel
            else throw new IllegalStateException("Core semiring's summary " + summ + " did not return either current " + currentSteps.weight + " or proposed " + fromThroughToSteps.weight + " weigt.")
          }
          else currentLabel
        }
        else fromThroughToLabel
      }

      def extend(fromThroughLabel: Label, throughToLabel: Label): Label = {
        //changing the match/case to if/else made this disappear from the sampling profiler
        if ((fromThroughLabel != O) && (throughToLabel != O)) {
          val fromThroughSteps: BrandesSteps[Node, CoreLabel] = fromThroughLabel.get
          val throughToSteps: BrandesSteps[Node, CoreLabel] = throughToLabel.get
          //if fromThroughLabel is identity, use throughToSteps. Otherwise the first step is fine
          val choices: Set[Node] = if (fromThroughLabel == I) throughToSteps.choices
          else fromThroughSteps.choices
          val choiceIndexes: Set[Int] = if (fromThroughLabel == I) throughToSteps.choiceIndexes
          else fromThroughSteps.choiceIndexes

          Some(new BrandesSteps[Node, CoreLabel](coreSupport.semiring.extend(fromThroughSteps.weight, throughToSteps.weight),
            fromThroughSteps.pathCount * throughToSteps.pathCount,
            choices,
            choiceIndexes
            ))
        }
        else O
      }
    }
  }
}