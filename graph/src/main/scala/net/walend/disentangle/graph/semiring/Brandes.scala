package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.{IndexedLabelDigraph, AdjacencyLabelDigraph}
import net.walend.disentangle.heap.{HeapOrdering, Heap, FibonacciHeap}
import scala.collection.{GenSeq, GenTraversable}

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */
//noinspection ReferenceMustBePrefixed

object Brandes {

  import scala.collection.mutable.Stack

  /**
   * Dijkstra's algorithm for a single sink, with a Seq of visited edges to support Brandes' algorithm.
   *
   * O(n ln(n) + a)
   */
  def brandesDijkstra[Node, Label, Key](labelDigraph: IndexedLabelDigraph[Node, Label],
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

    val allEdges = Dijkstra.dijkstraSingleSinkCustomHeap(labelDigraph, support)(sink, heap)
    val originEdgeStack: Stack[(labelDigraph.InnerNodeType, Label)] = stack.map(x => (x, allEdges(x.index)._3))

    (allEdges, originEdgeStack.filter(_._2 != support.semiring.O))
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
    val partialBetweenness: ArrayBuffer[Double] = ArrayBuffer.fill(labelGraph.nodeCount)(0.0)

    //for each possible choice of next step in the stack
    while (stack.nonEmpty) {
      val edge = stack.pop() //w
      //figure out the partial betweenness to apply to that step
      val label: Label = edge._2

      label.foreach(sourceLabel => {
        val sourceCount: Double = sourceLabel.pathCount
        val partialFromSource: Double = partialBetweenness(edge._1.index)
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
      })
    }

    partialBetweenness
  }


  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def allLeastPathsAndBetweenness[Node,
                                  CoreLabel,
                                  Key]
                                  (initialGraph: IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]],
                                   support: BrandesSupport[Node, CoreLabel, Key]): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {

    type Label = support.Label

    val innerNodes = initialGraph.innerNodes.asSeq

    val edgesAndBetweenParts: IndexedSeq[(IndexedSeq[(Node, Node, Label)], IndexedSeq[Double])] = for (sink <- innerNodes) yield {
      val edgeAndNodeStack: (IndexedSeq[(Node, Node, Label)], Stack[(initialGraph.InnerNodeType, Label)]) = brandesDijkstra(initialGraph, support)(sink)
      val partialB = partialBetweenness(support, initialGraph)(sink, edgeAndNodeStack._2, edgeAndNodeStack._1)
      val filteredEdges = edgeAndNodeStack._1.filter(_._3 != support.semiring.O)
      (filteredEdges, partialB)
    }

    def betweennessForNode(innerNode: initialGraph.InnerNodeType): Double = edgesAndBetweenParts.map(x => x._2(innerNode.index)).sum

    //noinspection ScalaUnnecessaryParentheses
    val betweennessMap: Map[Node, Double] = innerNodes.map(innerNode => (innerNode.value -> betweennessForNode(innerNode))).toMap

    val edges: IndexedSeq[(Node, Node, Label)] = edgesAndBetweenParts.flatMap(x => x._1)

    (edges, betweennessMap)
  }

  /**
   * Create a digraph of Labels from an edge list.
   *
   * @return an IndexedDigraph with graph's nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def createLabelDigraph[Node, EdgeLabel, CoreLabel, Key](edges: GenTraversable[(Node, Node, EdgeLabel)] = Seq.empty,
                                                         extraNodes: GenSeq[Node] = Seq.empty,
                                                         support: BrandesSupport[Node, CoreLabel, Key],
                                                         labelForEdge: (Node, Node, EdgeLabel) => CoreLabel): IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]] = {

    //Create the core label digraph to get everything's index
    val coreLabelDigraph:IndexedLabelDigraph[Node,CoreLabel] = Dijkstra.createLabelDigraph[Node,EdgeLabel,CoreLabel,Key](edges, extraNodes, support.coreSupport, labelForEdge)

    //Use that to create the Brandes labels
    val brandesEdges = coreLabelDigraph.innerEdges.map(x => (x._1.value,x._2.value,support.convertCoreLabelToLabel(coreLabelDigraph)(x)))

    AdjacencyLabelDigraph(brandesEdges,coreLabelDigraph.nodes.to[Seq],support.semiring.O)
  }

  def allLeastPathsAndBetweenness[Node, EdgeLabel, CoreLabel, Key](edges: GenTraversable[(Node, Node, EdgeLabel)],
                                                                  extraNodes: GenSeq[Node] = Seq.empty,
                                                                  coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                                  labelForEdge: (Node, Node, EdgeLabel) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {
    val support = new BrandesSupport[Node,CoreLabel,Key](coreSupport)
    val labelGraph: IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]] = createLabelDigraph(edges, extraNodes, support, labelForEdge)
    allLeastPathsAndBetweenness(labelGraph, support)
  }

  case class BrandesSteps[Node, CoreLabel](weight: CoreLabel, pathCount: Int, choiceIndexes:Seq[Int]) {

    /**
     * Overriding equals to speed up.
     */
    override def equals(any: Any) = {
      //noinspection TypeCheckCanBeMatch ,ComparingUnrelatedTypes - match too slow in profiler and benchmark
      if (any.isInstanceOf[BrandesSteps[Node, CoreLabel]]) {
        val other: BrandesSteps[Node, CoreLabel] = any.asInstanceOf[BrandesSteps[Node, CoreLabel]]
        if (this eq other) true //if they share a memory address, no need to compare
        else {
          if ((weight == other.weight) && (pathCount == other.pathCount)) {
            choiceIndexes == other.choiceIndexes
          } else false
        }
      } else false
    }

    /**
     * Overriding hashCode because I overrode equals.
     */
    override def hashCode(): Int = {
      weight.hashCode() ^ choiceIndexes.hashCode() ^ pathCount
    }
  }

  class BrandesSupport[Node, CoreLabel, Key](val coreSupport: SemiringSupport[CoreLabel, Key]) extends SemiringSupport[Option[BrandesSteps[Node, CoreLabel]], Key] {

    override type Label = Option[BrandesSteps[Node, CoreLabel]]

    def semiring: Semiring = BrandesSemiring

    def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

    def heapKeyForLabel:Label=>Key = _.fold(coreSupport.heapOrdering.AlwaysBottom)(x => coreSupport.heapKeyForLabel(x.weight))

    def convertCoreLabelToLabel(labelDigraph:IndexedLabelDigraph[Node,CoreLabel])
                         (edge:labelDigraph.InnerEdgeType): Label = {
      Option(BrandesSteps[Node, CoreLabel](edge._3, 1, Seq(edge._2.index)))
    }

    /**
     * A semiring for use with Brandes algorithm
     */
    object BrandesSemiring extends Semiring {

      def inDomain(label: Label): Boolean = {
        label.forall(steps => coreSupport.semiring.inDomain(steps.weight))
      }

      //identity and annihilator
      val I = Option(BrandesSteps[Node, CoreLabel](coreSupport.semiring.I, 1, Seq.empty))
      val O = None

      def summary(fromThroughToLabel: Label, currentLabel: Label): Label = {

        if (currentLabel != O) {
          if (fromThroughToLabel != O) {
            val currentSteps: BrandesSteps[Node, CoreLabel] = currentLabel.get
            val fromThroughToSteps: BrandesSteps[Node, CoreLabel] = fromThroughToLabel.get
            val summ = coreSupport.semiring.summary(fromThroughToSteps.weight, currentSteps.weight)
            if ((summ == fromThroughToSteps.weight) && (summ == currentSteps.weight)) {
              Option(BrandesSteps[Node, CoreLabel](currentSteps.weight,
                currentSteps.pathCount + fromThroughToSteps.pathCount,
                currentSteps.choiceIndexes ++ fromThroughToSteps.choiceIndexes))
            }
            else if (summ == fromThroughToSteps.weight) fromThroughToLabel
            else if (summ == currentSteps.weight) currentLabel
            else throw new IllegalStateException("Core semiring's summary " + summ + " did not return either current " + currentSteps.weight + " or proposed " + fromThroughToSteps.weight + " weight.")
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
          val choiceIndexes: Seq[Int] = if (fromThroughLabel == I) throughToSteps.choiceIndexes
                                        else fromThroughSteps.choiceIndexes

          Option(BrandesSteps[Node, CoreLabel](coreSupport.semiring.extend(fromThroughSteps.weight, throughToSteps.weight),
            fromThroughSteps.pathCount * throughToSteps.pathCount,
            choiceIndexes
            ))
        }
        else O
      }
    }
  }
}