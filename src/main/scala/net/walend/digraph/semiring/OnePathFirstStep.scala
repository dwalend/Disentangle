package net.walend.digraph.semiring

import net.walend.heap.HeapOrdering

/**
 * Finds one minimal path that use the core semiring.
 *
 * @author dwalend
 * @since v0.1.0
 */

case class FirstStep[Node,CoreLabel](weight:CoreLabel,step:Option[Node]) {
  /**
   * Overriding equals to speed up.
   */
  override def equals(any:Any) = {
    if (any.isInstanceOf[FirstStep[Node, CoreLabel]]) {
      val other: FirstStep[Node, CoreLabel] = any.asInstanceOf[FirstStep[Node, CoreLabel]]
      if (this eq other) true //if they share a memory address, no need to compare
      else {
        if (weight == other.weight) {
          step == other.step
        } else false
      }
    } else false
  }

  /**
   * Overriding hashCode because I overrode equals.
   */
  override def hashCode():Int = {
    weight.hashCode() ^ step.hashCode()
  }
}

class OnePathFirstStep[Node,CoreLabel,Key](coreSupport:SemiringSupport[CoreLabel,Key]) extends SemiringSupport[Option[FirstStep[Node,CoreLabel]],Key]{

  override type Label = Option[FirstStep[Node, CoreLabel]]

  def semiring: Semiring = OnePathSemiring

  def heapOrdering: HeapOrdering[Key] = coreSupport.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => coreSupport.heapKeyForLabel(nextStep.weight)
    case None => coreSupport.heapOrdering.AlwaysBottom
  }

  def convertArcToLabel[Arc](coreLabelForArc:(Node,Node,Arc)=>CoreLabel)
                              (start: Node, end: Node, arc: Arc):Label = {
    Some(FirstStep[Node,CoreLabel](coreLabelForArc(start,end,arc),Some(end)))
  }

  def convertArcToLabelFunc[Arc](coreLabelForArc:(Node,Node,Arc)=>CoreLabel):((Node,Node,Arc) => Label) = convertArcToLabel(coreLabelForArc)

  object OnePathSemiring extends Semiring {

    // todo report bug that I can't do this here
    // val coreSemiring = coreSupport.semiring

    def inDomain(label: Label): Boolean = {
      label match {
        case Some(step:FirstStep[Node,CoreLabel]) => coreSupport.semiring.inDomain(step.weight)
        case None => true
      }
    }

    //identity and annihilator
    val I = Some(FirstStep[Node,CoreLabel](coreSupport.semiring.I,None))
    val O = None

    def summary(fromThroughToLabel:Label,currentLabel:Label):Label = {

      if(currentLabel != O) {
        if(fromThroughToLabel != O){
          val currentStep:FirstStep[Node,CoreLabel] = currentLabel.get
          val fromThroughToStep:FirstStep[Node,CoreLabel] = fromThroughToLabel.get
          val summ = coreSupport.semiring.summary(fromThroughToStep.weight,currentStep.weight)
          if (summ==currentStep.weight) currentLabel
          else if (summ==fromThroughToStep.weight) fromThroughToLabel
          else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentStep.weight+" or proposed "+fromThroughToStep.weight+" weight.")
        }
        else currentLabel
      }
      else fromThroughToLabel
    }

    def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
      //changing the match/case to if/else made this disappear from the sampling profiler
      if((fromThroughLabel != O)&&(throughToLabel != O)) {
        val fromThroughStep:FirstStep[Node,CoreLabel] = fromThroughLabel.get
        val throughToStep:FirstStep[Node,CoreLabel] = throughToLabel.get
        //if fromThroughLabel is identity, use throughToSteps. Otherwise the first step is fine
        val step:Option[Node] = if(fromThroughLabel == I) throughToStep.step
        else fromThroughStep.step

        Some(new FirstStep[Node,CoreLabel](coreSupport.semiring.extend(fromThroughStep.weight,throughToStep.weight),step))
      }
      else O
    }
  }
}
