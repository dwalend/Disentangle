package net.walend.digraph.semiring

import net.walend.digraph.MutableLabelDigraph
import scala.collection.{GenSeq, GenTraversable}

/**
 * An implementation of the Floyd Warshall algorithm for general graph minimization.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FloydWarshall {

  /**
   * O(1)
   */
  def relax[Node,Label,Key](labelDigraph:MutableLabelDigraph[Node,Label],
                 semiring:SemiringSupport[Label,Key]#Semiring)
                (from:labelDigraph.InnerNodeType,
                 through:labelDigraph.InnerNodeType,
                 to:labelDigraph.InnerNodeType):Label = {
    val fromThrough:Label = labelDigraph.label(from,through)
    val throughTo:Label = labelDigraph.label(through,to)

    val current:Label = labelDigraph.label(from,to)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * O(n&#94;3)
   */
  def floydWarshall[Node,Label,Key](labelDigraph:MutableLabelDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableLabelDigraph[Node,Label] = {
    val innerNodes = labelDigraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      val summaryLabel = relax(labelDigraph,support.semiring)(i,k,j)
      labelDigraph.upsertArc(i,j,summaryLabel)
    }
    labelDigraph
  }

  /**
   * O(n&#94;3)
   */
  def allPairsShortestPaths[Node,Label,Key](labelDigraph:MutableLabelDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableLabelDigraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   *
   * O(n ln(n) + an)
   *
   * @return a Digraph with graph's nodes, a self-arc for each node with the semiring's identifier, and an arc for each arc specified by labelForArc.
   */
  def createLabelDigraph[Node,ArcLabel,Label,Key](arcs:GenTraversable[(Node,Node,ArcLabel)] = Seq.empty,
                                              extraNodes:GenSeq[Node] = Seq.empty,
                                              support:SemiringSupport[Label,Key],
                                              labelForArc:(Node,Node,ArcLabel)=>Label):MutableLabelDigraph[Node,Label] = {
    val nodes = (extraNodes ++ arcs.map(_._1) ++ arcs.map(_._2)).distinct
    val nonSelfArcs = arcs.filter(x => x._1 != x._2)
    val labelArcs = nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfArcs.map(x => (x._1,x._2,labelForArc(x._1,x._2,x._3)))

    import net.walend.digraph.MatrixLabelDigraph
    MatrixLabelDigraph(labelArcs,nodes,support.semiring.O)
  }

  /**
   * O(n&#94;3)
   */
  def allPairsShortestPaths[Node,ArcLabel,Label,Key](arcs:GenTraversable[(Node,Node,ArcLabel)] = Seq.empty,
                                                 extraNodes:GenSeq[Node] = Seq.empty,
                                                 support:SemiringSupport[Label,Key],
                                                 labelForArc:(Node,Node,ArcLabel)=>Label):MutableLabelDigraph[Node,Label] = {
    val initialDigraph = createLabelDigraph(arcs,extraNodes,support,labelForArc)
    floydWarshall(initialDigraph,support)
  }
}