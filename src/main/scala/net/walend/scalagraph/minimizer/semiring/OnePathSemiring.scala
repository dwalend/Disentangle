package net.walend.scalagraph.minimizer.semiring

/**
 * Finds a single minimal path using a core semiring.
 *
 * @author dwalend
 * @since v1
 */
case class Step[N,CL](weight:CL,next:Option[N]) {}

class OnePathSemiring[N,CL](coreSemiring:Semiring[CL]) extends Semiring[Option[Step[N,CL]]] {

  type Label = Option[Step[N,CL]]

  //identity and annihilator
  val I = Some(Step[N,CL](coreSemiring.I,None))
  val O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Label = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToStep),Some(currentStep)) => {
        val summ = coreSemiring.summary(fromThroughToStep.weight,currentStep.weight)
        //if a tie, stick with the current label
        if((summ==fromThroughToStep.weight)&&(summ==currentStep.weight)) {
          currentLabel
        }
        else if (summ==fromThroughToStep.weight) fromThroughToLabel
        else if (summ==currentStep.weight) currentLabel
        else throw new IllegalStateException("Core semiring's summary "+summ+" did not return either current "+currentStep.weight+" or proposed "+fromThroughToStep.weight+" weigt.")
      }
      case (Some(fromThroughToStep),O) => fromThroughToLabel
      case (O,Some(current)) => currentLabel
      case _ => O
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughStep),Some(throughToStep)) => {
        Some(new Step(coreSemiring.extend(fromThroughStep.weight,throughToStep.weight),fromThroughStep.next))
      }
      case _ => O
    }
  }
}

class OnePath[N,CL,Key](core:GraphMinimizerSupport[CL,Key]) extends GraphMinimizerSupport[Option[Step[N,CL]],Key] {
  def semiring = new OnePathSemiring(core.semiring)

  def heapOrdering = core.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => core.heapOrdering.AlwaysBottom
  }
}
