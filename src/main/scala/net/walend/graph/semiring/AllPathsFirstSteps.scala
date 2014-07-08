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

  def heapKeyForLabel = {
    case Some(nextStep) => coreSupport.heapKeyForLabel(nextStep.weight)
    case None => coreSupport.heapOrdering.AlwaysBottom
  }

  //todo could be a Seq instead if just used in Dijkstra's algorithm
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
    Some(FirstSteps(coreLabelForEdge(start,end,edgeLabel),Set(end)))
  }

  def convertEdgeToLabelFunc[EdgeLabel](coreLabelForEdge:(Node,Node,EdgeLabel)=>CoreLabel):((Node,Node,EdgeLabel) => Label) = convertEdgeToLabel(coreLabelForEdge)

  object AllPathsSemiring extends Semiring {

// todo report bug that I can't do this here
// val coreSemiring = coreSupport.semiring

    def inDomain(label: Label): Boolean = {
      label match {
        case Some(steps:FirstSteps) => coreSupport.semiring.inDomain(steps.weight)
        case None => true
      } 
    }
    
    //identity and annihilator
    val I = Some(FirstSteps(coreSupport.semiring.I,Set[Node]()))
    val O = None

    def summary(fromThroughToLabel:Label,currentLabel:Label):Label = {

      if(currentLabel != O) {
        if(fromThroughToLabel != O){
          val currentSteps:FirstStepsTrait[Node,CoreLabel] = currentLabel.get
          val fromThroughToSteps:FirstStepsTrait[Node,CoreLabel] = fromThroughToLabel.get
          val summ = coreSupport.semiring.summary(fromThroughToSteps.weight,currentSteps.weight)
          if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
            Some(new FirstSteps(currentSteps.weight,
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

        Some(new FirstSteps(coreSupport.semiring.extend(fromThroughSteps.weight,throughToSteps.weight),
                                            choices))
      }
      else O
    }
  }

  //todo all paths instead of just one

  def allLeastPaths(from:Node,to:Node)(leastPathDigraph:LabelDigraph[Node,Label]):Seq[Seq[leastPathDigraph.InnerNodeType]] = {

    type Path = Seq[leastPathDigraph.InnerNodeType]

    def leastPathsOfInnerNodes(fromInner:Option[leastPathDigraph.InnerNodeType],
                              toInner:Option[leastPathDigraph.InnerNodeType]):Seq[Path] = {
      (fromInner,toInner) match {
        case (Some(f),Some(t)) => {
          val label:Label = leastPathDigraph.label(f,t)
          label match {
            case Some(firstStep) => {
              if(firstStep.choices == Set.empty) Seq(Seq.empty[leastPathDigraph.InnerNodeType]) //No further steps. from should be to and the label should be I
              else {
                //todo change to choice -> sequence of tails

                for (choice <- firstStep.choices.to[Seq]) yield {
                  val tails: Seq[Path] = leastPathsOfInnerNodes(leastPathDigraph.innerNode(choice), toInner)
                  println(tails)

                  for (tail <- tails) yield {
                    val innerStep: leastPathDigraph.InnerNodeType = leastPathDigraph.innerNode(choice).get
                    Seq(innerStep +: tail)
                  }
                }.flatten
              }.flatten
            }
            case None => Seq() //No path from one to the other
          }
        }
        case _ => Seq() //One node or the other isn't in the graph
      }
    }

    val fromInner = leastPathDigraph.innerNode(from)
    val toInner = leastPathDigraph.innerNode(to)

    //prepend fromInner to all?
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
      label match {
        case Some(firstSteps) => {

          val innerChoices = firstSteps.choices.map(choice => labelGraph.innerNode(choice).get)
          val closeEdges:Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = innerChoices.map((innerFrom,_,label))

          val farEdges:Set[(labelGraph.InnerNodeType,labelGraph.InnerNodeType,Label)] = innerChoices.map(recurse(_,innerTo)).flatten

          closeEdges ++ farEdges
        }
        case None => Set.empty
      }
    }
    val innerFrom = labelGraph.innerNode(from).getOrElse(throw new IllegalArgumentException(s"$from not in labelGraph"))

    recurse(innerFrom,innerTo)
  }
}

trait FirstStepsTrait[Node,CoreLabel] {

  def weight:CoreLabel

  //todo rename choiceSeq
  def choices:Set[Node]

}