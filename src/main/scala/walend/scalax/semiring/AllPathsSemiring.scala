package walend.scalax.semiring


/**
 * Finds all minimal paths that use the core semiring.
 *
 * @author dwalend
 * @since v1
 */

case class Steps[N,CL](weight:CL,choices:Set[N]) {}

class AllPathsSemiring[N,CL](coreSemiring:Semiring[CL]) extends Semiring[Option[Steps[N,CL]]] {

  type Label = Option[Steps[N,CL]]

  //identity and annihilator
  val I = Some(Steps[N,CL](coreSemiring.I,Set[N]()))
  val O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Label = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        val summ = coreSemiring.summary(fromThroughToSteps.weight,currentSteps.weight)
        if((summ==fromThroughToSteps.weight)&&(summ==currentSteps.weight)) {
          Some(new Steps[N,CL](currentSteps.weight,currentSteps.choices ++ fromThroughToSteps.choices))
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

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {
        Some(new Steps[N,CL](coreSemiring.extend(fromThroughSteps.weight,throughToSteps.weight),fromThroughSteps.choices))
      }
      case _ => O
    }
  }
}

class AllPaths[N,CL,Key](core:GraphMinimizerSupport[CL,Key]) extends GraphMinimizerSupport[Option[Steps[N,CL]],Key] {
  def semiring = new AllPathsSemiring(core.semiring)

  def heapOrdering = core.heapOrdering

  def heapKeyForLabel = {
    case Some(nextStep) => nextStep.weight.asInstanceOf[Key]
    case None => core.heapOrdering.AlwaysBottom
  }
}
