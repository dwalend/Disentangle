package walend.scalax.semiring

/**
 * Labels are true if the sink can be reached from the source, false if not.
 *
 * @author dwalend
 * @since v1
 */
object TransitiveClosureSemiring extends Semiring[Boolean] {


  //identity and annihilator
  def I = true
  def O = false

  def summary(fromThroughToLabel:Boolean,
              currentLabel:Boolean):Boolean = {
    fromThroughToLabel | currentLabel
  }

  def extend(fromThroughLabel:Boolean,throughToLabel:Boolean):Boolean = {
    fromThroughLabel & throughToLabel
  }

}

object TransitiveClosureLabelGraphBuilder extends LabelGraphBuilder[Boolean] {
  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+> edge._2)(true)
  }
}

