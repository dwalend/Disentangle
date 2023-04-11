package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.{AdjacencyLabelDigraph, IndexedLabelDigraph}
import net.walend.disentangle.heap.{FibonacciHeap, Heap, HeapOrdering}

import scala.collection.immutable.{Iterable, Seq}

/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */
//noinspection ReferenceMustBePrefixed

object Brandes {

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

    IndexedSeq.from(partialBetweenness)
  }

  /**
   * Create a digraph of Labels from an edge list.
   *
   * @return an IndexedDigraph with graph's nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def createLabelDigraph[Node, EdgeLabel, CoreLabel, Key](edges: Iterable[(Node, Node, EdgeLabel)] = Seq.empty,
                                                         nodeOrder: Seq[Node] = Seq.empty,
                                                         support: BrandesSupport[Node, CoreLabel, Key],
                                                         labelForEdge: (Node, Node, EdgeLabel) => CoreLabel): IndexedLabelDigraph[Node, Option[BrandesSteps[Node, CoreLabel]]] = {

    val nodes = (nodeOrder ++ edges.map(_._1) ++ edges.map(_._2)).distinct
    val nonSelfEdges = edges.filter(x => x._1 != x._2)
    val labelEdges: Seq[(Node, Node, CoreLabel)] = nodes.map(x => (x,x,support.coreSupport.semiring.I)) ++
      nonSelfEdges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    //Use that to create the Brandes labels
    val brandesEdges = labelEdges.map(x => (x._1,x._2,support.convertCoreLabelToLabel(x._3,nodes.indexOf(x._2))))

    AdjacencyLabelDigraph(brandesEdges,nodes,support.semiring.O)
  }

  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def allLeastPathsAndBetweenness[Node, EdgeLabel, CoreLabel, Key](edges: Iterable[(Node, Node, EdgeLabel)],
                                                                  nodeOrder: Seq[Node] = Seq.empty,
                                                                  coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                                  labelForEdge: (Node, Node, EdgeLabel) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {
    val support = new BrandesSupport[Node,CoreLabel,Key](coreSupport)
    type Label = support.Label

    val initialGraph: IndexedLabelDigraph[Node,Label] = createLabelDigraph(edges, nodeOrder, support, labelForEdge)

    val innerNodes = initialGraph.innerNodes.asSeq

    val edgesAndBetweenParts: IndexedSeq[(IndexedSeq[(Node, Node, Label)], IndexedSeq[Double])] = for (sink <- innerNodes) yield {
      val edgeAndNodeStack = brandesDijkstra(initialGraph, support)(sink)
      val partialB = partialBetweenness(support, initialGraph)(sink, edgeAndNodeStack._2, edgeAndNodeStack._1)
      val filteredEdges = edgeAndNodeStack._1.filter(_._3 != support.semiring.O)
      (filteredEdges, partialB)
    }

    def betweennessForNode(innerNode: initialGraph.InnerNodeType): Double = edgesAndBetweenParts.map(x => x._2(innerNode.index)).sum

    //noinspection ScalaUnnecessaryParentheses
    val betweennessMap: Map[Node, Double] = innerNodes.map(innerNode => (innerNode.value -> betweennessForNode(innerNode))).toMap

    val shortPaths: IndexedSeq[(Node, Node, Label)] = edgesAndBetweenParts.flatMap(x => x._1)

    (shortPaths, betweennessMap)
  }
  
  case class BrandesSteps[Node, CoreLabel](weight: CoreLabel, pathCount: Int, choiceIndexes:Seq[Int]) {

    /**
     * Overriding equals to speed up.
     */
    override def equals(any: Any): Boolean = {
      //noinspection TypeCheckCanBeMatch ,ComparingUnrelatedTypes - match too slow in profiler and benchmark
      any match {
        case other:BrandesSteps[_, _] =>
          if (this eq other) true
          else {
            if ((weight == other.weight) && (pathCount == other.pathCount)) {
              choiceIndexes == other.choiceIndexes
            } else false
          }
        case _ => false
      }
    }

    /**
     * Overriding hashCode because I overrode equals.
     */
    override def hashCode(): Int = {
      weight.hashCode() ^ choiceIndexes.hashCode() ^ pathCount
    }
  }

  case class BrandesSupport[Node, CoreLabel, Key](coreSupport: SemiringSupport[CoreLabel, Key]) extends SemiringSupport[Option[BrandesSteps[Node, CoreLabel]], Key] {

    override type Label = Option[BrandesSteps[Node, CoreLabel]]

    def semiring: Semiring = BrandesSemiring

    def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

    def heapKeyForLabel:Label=>Key = _.fold(coreSupport.heapOrdering.AlwaysBottom)(x => coreSupport.heapKeyForLabel(x.weight))

    def convertCoreLabelToLabel(coreLabel:CoreLabel,toIndex:Int): Label = {
      Option(BrandesSteps[Node, CoreLabel](coreLabel, 1, Seq(toIndex)))
    }

    /**
     * A semiring for use with Brandes algorithm
     */
    object BrandesSemiring extends Semiring {

      def inDomain(label: Label): Boolean = {
        label.forall(steps => coreSupport.semiring.inDomain(steps.weight))
      }

      //identity and annihilator
      val I: Label = Option(BrandesSteps[Node, CoreLabel](coreSupport.semiring.I, 1, Seq.empty))
      val O: Label = None

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

    /**
     * Create the acyclic subgraph defined by BrandesSupport
     */
    //todo another spot for an insert-ordered set
    def subgraphEdges(labelGraph:IndexedLabelDigraph[Node,Label],from:Node,to:Node):Set[labelGraph.InnerEdgeType] = {

      val innerTo = labelGraph.innerNode(to).getOrElse(throw new IllegalArgumentException(s"$to not in labelGraph"))
      val innerFrom = labelGraph.innerNode(from).getOrElse(throw new IllegalArgumentException(s"$from not in labelGraph"))

      //todo capture visited nodes and don't revisit them, by taking innerFrom as a Set, pulling out bits, passing in Sets of novel nodes to visit, and passing around another set of nodes already visited.
      def recurse(innerFrom:labelGraph.InnerNodeType,innerTo:labelGraph.InnerNodeType):Seq[labelGraph.InnerEdgeType] = {

        val edge = labelGraph.edge(innerFrom,innerTo)
        //if the label is None then return an empty set
        //otherwise, follow the choices
        edge.fold(Seq.empty[labelGraph.InnerEdgeType])(firstSteps => {
          val innerChoices: Seq[labelGraph.InnerNodeType] = firstSteps.label.get.choiceIndexes.map(choice => labelGraph.innerNodeForIndex(choice))
          val closeEdges:Seq[labelGraph.InnerEdgeType] = innerChoices.map(labelGraph.edge(innerFrom,_).get)
          val farEdges:Seq[labelGraph.InnerEdgeType] = innerChoices.flatMap(recurse(_, innerTo))

          closeEdges ++ farEdges
        })
      }

      Set.from(recurse(innerFrom,innerTo))
    }

    def allLeastPaths(leastPathDigraph:IndexedLabelDigraph[Node,Label],from:Node,to:Node):Seq[Seq[leastPathDigraph.InnerNodeType]] = {

      type Path = Seq[leastPathDigraph.InnerNodeType]

      def leastPathsOfInnerNodes(fromInner:Option[leastPathDigraph.InnerNodeType],
                                 toInner:Option[leastPathDigraph.InnerNodeType]):Seq[Path] = {
        val fromToOption: Option[(leastPathDigraph.InnerNodeType, leastPathDigraph.InnerNodeType)] = for (f <- fromInner; t <- toInner) yield (f, t)
        //If fromToOption is None, then one node or the other isn't in the graph. Return no Path
        fromToOption.fold(Seq.empty[Path])(fromTo => {
          val label: Label = leastPathDigraph.label(fromTo._1, fromTo._2)
          //If label is None, then there's no path from one node to the other. Return no Path
          label.fold(Seq.empty[Path])(firstSteps => {
            if (firstSteps.choiceIndexes == Seq.empty) Seq(Seq(fromTo._2)) //No further steps. from should be to and the label should be I
            else {
              //Follow each choice and prepend the node this starts from
              for (choiceIndex <- firstSteps.choiceIndexes) yield {
                val tails: Seq[Path] = leastPathsOfInnerNodes(Some(leastPathDigraph.innerNodeForIndex(choiceIndex)), toInner)
                for (tail <- tails) yield {
                  Seq(fromTo._1 +: tail)
                }
              }.flatten
            }.flatten
          })
        })
      }

      val fromInner = leastPathDigraph.innerNode(from)
      val toInner = leastPathDigraph.innerNode(to)

      leastPathsOfInnerNodes(fromInner,toInner)
    }
  }

  object BrandesSupport {
    def apply[Node]():BrandesSupport[Node,Int,Int] = new BrandesSupport[Node,Int,Int](FewestNodes)
  }

}

case class Stack[A](var list:List[A] = List.empty) {

  def push(elem: A): this.type = { list = elem :: list; this }

  def map[B](f: A => B): Stack[B] = Stack(list.map(f))

  def filter(p: A => Boolean): Stack[A] = Stack(list.filter(p))

  def nonEmpty: Boolean = list.nonEmpty

  def pop(): A = {
    val result = list.head
    list = list.tail
    result
  }
}