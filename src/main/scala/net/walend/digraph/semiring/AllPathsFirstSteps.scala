package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v0.1.0
 */

case class FirstSteps[Node,CoreLabel](weight:CoreLabel,choices:Set[Node]) {}

class AllPathsFirstSteps[Node,CoreLabel,Key](coreSupport:SemiringSupport[CoreLabel,Key]) extends SemiringSupport[Option[FirstSteps[Node,CoreLabel]],Key]{
  
  override type Label = Option[FirstSteps[Node, CoreLabel]]

  def semiring: Semiring = AllPathsSemiring

  def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => coreSupport.heapOrdering.AlwaysBottom
  }

  def convertEdgeToLabel[Edge](coreLabelForEdge:(Node,Node,Edge)=>CoreLabel)
                              (start: Node, end: Node, edge: Edge):Label = {
    Some(FirstSteps[Node,CoreLabel](coreLabelForEdge(start,end,edge),Set(end)))
  }

  def convertEdgeToLabelFunc[Edge](coreLabelForEdge:(Node,Node,Edge)=>CoreLabel):((Node,Node,Edge) => Label) = convertEdgeToLabel(coreLabelForEdge)

  object AllPathsSemiring extends Semiring {

// todo report bug that I can't do this here
// val coreSemiring = coreSupport.semiring

    def inDomain(label: Label): Boolean = {
      label match {
        case Some(steps:FirstSteps[Node,CoreLabel]) => coreSupport.semiring.inDomain(steps.weight)
        case None => true
      } 
    }
    
    //identity and annihilator
    val I = Some(FirstSteps[Node,CoreLabel](coreSupport.semiring.I,Set[Node]()))
    val O = None

    def summary(fromThroughToLabel:Label,currentLabel:Label):Label = {

      if(currentLabel != O) {
        if(fromThroughToLabel != O){
          val currentSteps:FirstSteps[Node,CoreLabel] = currentLabel.get
          val fromThroughToSteps:FirstSteps[Node,CoreLabel] = fromThroughToLabel.get
          val summ = coreSupport.semiring.summary(fromThroughToSteps.weight,currentSteps.weight)
          if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
            Some(new FirstSteps[Node,CoreLabel](currentSteps.weight,currentSteps.choices ++ fromThroughToSteps.choices))
          }
          else if (summ==fromThroughToSteps.weight) fromThroughToLabel
          else if (summ==currentSteps.weight) currentLabel
          else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentSteps.weight+" or proposed "+fromThroughToSteps.weight+" weigt.")
        }
        else currentLabel
      }
      else fromThroughToLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      //changing the match/case to if/else made this disappear from the sampling profiler
      if((fromThroughLabel != O)&&(throughToLabel != O)) {
        val fromThroughSteps:FirstSteps[Node,CoreLabel] = fromThroughLabel.get
        val throughToSteps:FirstSteps[Node,CoreLabel] = throughToLabel.get
        Some(new FirstSteps[Node,CoreLabel](coreSupport.semiring.extend(fromThroughSteps.weight,throughToSteps.weight),fromThroughSteps.choices))
      }
      else O
    }
  } 
}
