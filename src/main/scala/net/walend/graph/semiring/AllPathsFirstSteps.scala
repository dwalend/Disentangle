package net.walend.graph.semiring

import net.walend.heap.HeapOrdering
import net.walend.graph.LabelDigraph

/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v0.1.0
 */
class AllPathsFirstSteps[Node,CoreLabel,Key](coreSupport:SemiringSupport[CoreLabel,Key]) extends SemiringSupport[Option[FirstStepsTrait[Node,CoreLabel]],Key]{
  
  override type Label = Option[FirstStepsTrait[Node, CoreLabel]]

  def semiring: Semiring = AllPathsSemiring

  def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

  def heapKeyForLabel:Label=>Key = _.fold(coreSupport.heapOrdering.AlwaysBottom)(x => coreSupport.heapKeyForLabel(x.weight))

  //todo could be a Seq instead if just used in Dijkstra's algorithm -- faster
  //todo or another place to use the IndexedSet.
  case class FirstSteps(weight:CoreLabel,choices:Set[Node]) extends FirstStepsTrait[Node, CoreLabel] {

    /**
     * Overriding equals to speed up.
     */
    override def equals(any:Any) = {
      if (any.isInstanceOf[FirstSteps]) {
        val other: FirstSteps = any.asInstanceOf[FirstSteps]
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
    override def hashCode():Int = {
      weight.hashCode() ^ choices.hashCode()
    }
  }


  def convertEdgeToLabel[EdgeLabel](coreLabelForEdge:(Node,Node,EdgeLabel)=>CoreLabel)
                              (start: Node, end: Node, edgeLabel: EdgeLabel):Label = {
    Option(FirstSteps(coreLabelForEdge(start,end,edgeLabel),Set(end)))
  }

  def convertEdgeToLabelFunc[EdgeLabel](coreLabelForEdge:(Node,Node,EdgeLabel)=>CoreLabel):((Node,Node,EdgeLabel) => Label) = convertEdgeToLabel(coreLabelForEdge)

  object AllPathsSemiring extends Semiring {

    val coreSemiring:SemiringSupport[CoreLabel,Key]#Semiring = coreSupport.semiring

    def inDomain(label: Label): Boolean = {
      label.forall(steps => coreSemiring.inDomain(steps.weight))
    }
    
    //identity and annihilator
    val I = Option(FirstSteps(coreSemiring.I,Set[Node]()))
    val O = None

    def summary(fromThroughToLabel:Label,currentLabel:Label):Label = {

      if(currentLabel != O) {
        if(fromThroughToLabel != O){
          val currentSteps:FirstStepsTrait[Node,CoreLabel] = currentLabel.get
          val fromThroughToSteps:FirstStepsTrait[Node,CoreLabel] = fromThroughToLabel.get
          val summ = coreSemiring.summary(fromThroughToSteps.weight,currentSteps.weight)
          if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
            Option(FirstSteps(currentSteps.weight,
                                currentSteps.choices ++ fromThroughToSteps.choices))
          }
          else if (summ==fromThroughToSteps.weight) fromThroughToLabel
          else if (summ==currentSteps.weight) currentLabel
          else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentSteps.weight+" or proposed "+fromThroughToSteps.weight+" weight.")
        }
        else currentLabel
      }
      else fromThroughToLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      //changing the match/case to if/else made this disappear from the sampling profiler
      if((fromThroughLabel != O)&&(throughToLabel != O)) {
        val fromThroughSteps:FirstStepsTrait[Node,CoreLabel] = fromThroughLabel.get
        val throughToSteps:FirstStepsTrait[Node,CoreLabel] = throughToLabel.get
        //if fromThroughLabel is identity, use throughToSteps. Otherwise the first step is fine
        val choices:Set[Node] = if(fromThroughLabel == I) throughToSteps.choices
                                else fromThroughSteps.choices

        Option(FirstSteps(coreSemiring.extend(fromThroughSteps.weight,throughToSteps.weight),
                              choices))
      }
      else O
    }
  }

  def allLeastPaths(from:Node,to:Node)(leastPathDigraph:LabelDigraph[Node,Label]):Seq[Seq[leastPathDigraph.InnerNodeType]] = {

    type Path = Seq[leastPathDigraph.InnerNodeType]

    def leastPathsOfInnerNodes(fromInner:Option[leastPathDigraph.InnerNodeType],
                              toInner:Option[leastPathDigraph.InnerNodeType]):Seq[Path] = {
      val fromToOption: Option[(leastPathDigraph.InnerNodeType, leastPathDigraph.InnerNodeType)] = for (f <- fromInner; t <- toInner) yield (f, t)
      //If fromToOption is None, then one node or the other isn't in the graph. Return no Path
      fromToOption.fold(Seq.empty[Path])(fromTo => {
        val label: Label = leastPathDigraph.label(fromTo._1, fromTo._2)
        //If label is None, then there's no path from one node to the other. Return no Path
        label.fold(Seq.empty[Path])(firstSteps => {
          if (firstSteps.choices == Set.empty) Seq(Seq(fromTo._2)) //No further steps. from should be to and the label should be I
          else {
            //Follow each choice and prepend the node this starts from
            for (choice <- firstSteps.choices.to[Seq]) yield {
              val tails: Seq[Path] = leastPathsOfInnerNodes(leastPathDigraph.innerNode(choice), toInner)
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
  /**
   * Create the acyclic subgraph defined by AllPathsFirstSteps
   */
  //todo another spot for an insert-ordered set
  def subgraphEdges(labelGraph:LabelDigraph[Node,Label],from:Node,to:Node):Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = {

    val innerTo = labelGraph.innerNode(to).getOrElse(throw new IllegalArgumentException(s"$to not in labelGraph"))
    //todo capture visited nodes and don't revisit them, by taking innerFrom as a Set, pulling out bits, passing in Sets of novel nodes to visit, and passing around another set of nodes already visited.
    def recurse(innerFrom:labelGraph.InnerNodeType,innerTo:labelGraph.InnerNodeType):Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = {
      val label:Label = labelGraph.label(innerFrom,innerTo)

      //if the label is None then return an empty set
      //otherwise, follow the choices
      label.fold(Set.empty[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)])(firstSteps => {
        val innerChoices = firstSteps.choices.map(choice => labelGraph.innerNode(choice).get)
        val closeEdges:Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = innerChoices.map((innerFrom,_,label))

        val farEdges:Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = innerChoices.map(recurse(_,innerTo)).flatten

        closeEdges ++ farEdges
      })
    }
    val innerFrom = labelGraph.innerNode(from).getOrElse(throw new IllegalArgumentException(s"$from not in labelGraph"))

    recurse(innerFrom,innerTo)
  }
}

trait FirstStepsTrait[Node,CoreLabel] {

  def weight:CoreLabel

  def choices:Set[Node]

}