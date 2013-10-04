package walend.scalax.semiring

/**
 * Finds the length of a path that traverses the fewest edges.
 *
 * @author dwalend
 * @since v1
 */
object CountFewestNodesSemiring extends Semiring[Int] {

  //identity and annihilator
  def I = 0
  def O = Int.MaxValue

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Int,
              currentLabel:Int):Option[Int] = {
    if(fromThroughToLabel < currentLabel) {
      Some(fromThroughToLabel)
    }
    else None
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Int,throughToLabel:Int):Int = {
    fromThroughLabel + throughToLabel
  }
}

object CountFewestNodesBetweenGraphBuilder extends LabelGraphBuilder[Int] {

  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn

    import scalax.collection.edge.Implicits._
    (edge._1 ~+> edge._2)(1)
  }
}

