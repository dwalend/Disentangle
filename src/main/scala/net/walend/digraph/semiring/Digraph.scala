package net.walend.digraph.semiring

/**
 * Methods required for a Digraph representation for the algorithms.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait Digraph[Node,Edge] {

  trait InnerNodeTrait {
    def value:Node
    /* todo after FloydWarshall
    def successors:Seq[InnerNode]

    def predecessors:Seq[InnerNode]
    */
  }

  type InnerNodeType <: InnerNodeTrait

  def innerNodes:Seq[InnerNodeType]

  def innerNode(value:Node):Option[InnerNodeType]

  def noEdgeExistsValue:Edge

  def edge(from:InnerNodeType,to:InnerNodeType):Edge

  def updateEdge(from:InnerNodeType,to:InnerNodeType,edge:Edge):Unit
  //todo toString, after FloydWarshall
}
