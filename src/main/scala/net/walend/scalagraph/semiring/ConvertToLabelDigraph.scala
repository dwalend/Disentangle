package net.walend.scalagraph.semiring

import net.walend.digraph.semiring.SemiringSupport

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.Graph

/**
 * Convert a scala-graph Graph to a Digraph of Labels.
 *
 * @author dwalend
 * @since v0.1.0
 */
object ConvertToLabelDigraph {

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   *
   * @return label arcs, nodes, and support.semiring.O to build a net.walend.digraph.Digraph.
   */
  import scala.language.higherKinds
  def convert[Node,E[X] <: EdgeLikeIn[X],Label,Key](originalGraph:Graph[Node,E],
                                                    support:SemiringSupport[Label,Key])(
                                                    labelForEdge:originalGraph.EdgeT=>(Node,Node,Label)):(Seq[(Node,Node,Label)],Seq[Node],Label) = {

    val nodes:Seq[Node] = originalGraph.nodes.toOuter.to[Seq]

    val identityArcs:Seq[(Node,Node,Label)] = nodes.map(x => (x,x,support.semiring.I))
    val graphArcs:Seq[(Node,Node,Label)] = originalGraph.edges.map(labelForEdge).to[Seq].filter(x => x._1 != x._2) //no self-edges

    val arcs:Seq[(Node,Node,Label)] = identityArcs ++ graphArcs

    (arcs,nodes,support.semiring.O)
  }

  //todo labelForEdge for all of the different scala-graph edges
}
