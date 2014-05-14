package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v0.1.0
 */

case class FirstSteps[Node,CoreLabel](weight:CoreLabel,choices:Set[Node]) {}

class AllPathsFirstSteps[Node,CoreLabel,Key](coreSupport:GraphMinimizerSupport[CoreLabel,Key]) extends GraphMinimizerSupport[Option[FirstSteps[Node,CoreLabel]],Key]{
  
  override type Label = Option[FirstSteps[Node, CoreLabel]]

  def semiring: Semiring = AllPathsSemiring

  def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => coreSupport.heapOrdering.AlwaysBottom
  }

  object AllPathsSemiring extends Semiring {

// todo report bug that I can't do this   val coreSemiring = coreSupport.semiring

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

      (fromThroughToLabel,currentLabel) match {
        case (Some(fromThroughToSteps),Some(currentSteps)) => {
          val summ = coreSupport.semiring.summary(fromThroughToSteps.weight,currentSteps.weight)
          if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
            Some(new FirstSteps[Node,CoreLabel](currentSteps.weight,currentSteps.choices ++ fromThroughToSteps.choices))
          }
          else if (summ==fromThroughToSteps.weight) fromThroughToLabel
          else if (summ==currentSteps.weight) currentLabel
          else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentSteps.weight+" or proposed "+fromThroughToSteps.weight+" weigt.")
        }
        case (Some(fromThroughToNodes),O) => fromThroughToLabel
        case (O,Some(current)) => currentLabel
        case _ => O
      }
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {

      (fromThroughLabel,throughToLabel) match {
        case (Some(fromThroughSteps),Some(throughToSteps)) => {
          Some(new FirstSteps[Node,CoreLabel](coreSupport.semiring.extend(fromThroughSteps.weight,throughToSteps.weight),fromThroughSteps.choices))
        }
        case _ => O
      }
    }
  } 
}
