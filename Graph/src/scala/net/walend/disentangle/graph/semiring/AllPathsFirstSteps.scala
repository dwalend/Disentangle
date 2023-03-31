package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.{IndexedLabelDigraph, LabelDigraph}
import net.walend.disentangle.heap.HeapOrdering

/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v0.1.0
 */
case class AllPathsFirstSteps[Node,CoreLabel,Key](coreSupport:SemiringSupport[CoreLabel,Key]) extends SemiringSupport[Option[FirstStepsTrait[Node,CoreLabel]],Key]{
  
  override type Label = Option[FirstStepsTrait[Node, CoreLabel]]

  def semiring: Semiring = AllPathsSemiring

  def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

  def heapKeyForLabel:Label=>Key = _.fold(coreSupport.heapOrdering.AlwaysBottom)(x => coreSupport.heapKeyForLabel(x.pathWeight))

  //todo could be a Seq instead if just used in Dijkstra's algorithm -- faster
  //todo or another place to use the IndexedSet.
  case class FirstSteps(pathWeight:CoreLabel,choices:Set[Node]) extends FirstStepsTrait[Node, CoreLabel] {

    /**
     * Overriding equals to speed up.
     */
    override def equals(any:Any) = {
      if (any.isInstanceOf[FirstSteps]) {
        val other: FirstSteps = any.asInstanceOf[FirstSteps]
        if (this eq other) true //if they share a memory address, no need to compare
        else {
          if (pathWeight == other.pathWeight) {
            choices == other.choices
          } else false
        }
      } else false
    }

    /**
     * Overriding hashCode because I overrode equals.
     */
    override def hashCode():Int = {
      pathWeight.hashCode() ^ choices.hashCode()
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
      label.forall(steps => coreSemiring.inDomain(steps.pathWeight))
    }
    
    //identity and annihilator
    val I = Option(FirstSteps(coreSemiring.I,Set[Node]()))
    val O = None

    def summary(fromThroughToLabel:Label,currentLabel:Label):Label = {

      if(currentLabel != O) {
        if(fromThroughToLabel != O){
          val currentSteps:FirstStepsTrait[Node,CoreLabel] = currentLabel.get
          val fromThroughToSteps:FirstStepsTrait[Node,CoreLabel] = fromThroughToLabel.get
          val summ = coreSemiring.summary(fromThroughToSteps.pathWeight,currentSteps.pathWeight)
          if((summ==fromThroughToSteps.pathWeight)&&(summ==currentSteps.pathWeight)) {
            Option(FirstSteps(currentSteps.pathWeight,
                                currentSteps.choices ++ fromThroughToSteps.choices))
          }
          else if (summ==fromThroughToSteps.pathWeight) fromThroughToLabel
          else if (summ==currentSteps.pathWeight) currentLabel
          else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentSteps.pathWeight+" or proposed "+fromThroughToSteps.pathWeight+" weight.")
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

        Option(FirstSteps(coreSemiring.extend(fromThroughSteps.pathWeight,throughToSteps.pathWeight),
                              choices))
      }
      else O
    }
  }

  def allLeastPaths(leastPathDigraph:LabelDigraph[Node,Label],from:Node,to:Node):Seq[Seq[leastPathDigraph.InnerNodeType]] = {

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
            for (choice <- Seq.from(firstSteps.choices)) yield {
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
    * Create the acyclic subgraph defined by BrandesSupport
    */
  //todo another spot for an insert-ordered set
  def subgraphEdges(labelGraph:IndexedLabelDigraph[Node,Label],from:Node,to:Node):Set[labelGraph.InnerEdgeType] = {

    val innerTo = labelGraph.innerNode(to).getOrElse(throw new IllegalArgumentException(s"$to not in labelGraph"))
    val innerFrom = labelGraph.innerNode(from).getOrElse(throw new IllegalArgumentException(s"$from not in labelGraph"))

    //todo capture visited nodes and don't revisit them, by taking innerFrom as a Set, pulling out bits, passing in Sets of novel nodes to visit, and passing around another set of nodes already visited.
    def recurse(innerFrom:labelGraph.InnerNodeType,innerTo:labelGraph.InnerNodeType):Set[labelGraph.InnerEdgeType] = {

      val edge = labelGraph.edge(innerFrom,innerTo)
      //if the label is None then return an empty set
      //otherwise, follow the choices
      edge.fold(Set.empty[labelGraph.InnerEdgeType])(firstSteps => {
        val innerChoices: Set[labelGraph.InnerNodeType] = firstSteps.label.get.choices.map(choice => labelGraph.innerNode(choice).get)
        val closeEdges:Set[labelGraph.InnerEdgeType] = innerChoices.map(labelGraph.edge(innerFrom,_).get)
        val farEdges:Set[labelGraph.InnerEdgeType] = innerChoices.flatMap(recurse(_, innerTo))

        closeEdges ++ farEdges
      })
    }

    recurse(innerFrom,innerTo)
  }
}

trait FirstStepsTrait[Node,CoreLabel] {

  def pathWeight:CoreLabel

  def choices:Set[Node]

}