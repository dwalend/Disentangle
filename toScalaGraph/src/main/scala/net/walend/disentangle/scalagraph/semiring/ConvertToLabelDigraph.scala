package net.walend.disentangle.scalagraph.semiring

import net.walend.graph.semiring.SemiringSupport

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge


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
   * @return label edges, nodes, and support.semiring.O to build a net.walend.graph.Digraph.
   */
  import scala.language.higherKinds
  def convert[Node,E[X] <: EdgeLikeIn[X],Label,Key](originalGraph:Graph[Node,E],
                                                    support:SemiringSupport[Label,Key])(
                                                    labelForEdge:E[Node]=>(Node,Node,Label)):(Seq[(Node,Node,Label)],Seq[Node],Label) = {

    val nodes:Seq[Node] = originalGraph.nodes.toOuter.to[Seq]

    val identityEdges:Seq[(Node,Node,Label)] = nodes.map(x => (x,x,support.semiring.I))
    val graphEdges:Seq[(Node,Node,Label)] = originalGraph.edges.map(x => labelForEdge(x.toOuter)).to[Seq].filter(y => y._1 != y._2) //no self-edges

    val edges:Seq[(Node,Node,Label)] = identityEdges ++ graphEdges

    (edges,nodes,support.semiring.O)
  }

}
