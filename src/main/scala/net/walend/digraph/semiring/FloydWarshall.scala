package net.walend.digraph.semiring

import net.walend.digraph.MutableArcDigraph

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
  def relax[Node,Arc,Key](labelDigraph:MutableArcDigraph[Node,Arc],
                 semiring:SemiringSupport[Arc,Key]#Semiring)
                (from:labelDigraph.InnerNodeType,
                 through:labelDigraph.InnerNodeType,
                 to:labelDigraph.InnerNodeType):Arc = {
    val fromThrough:Arc = labelDigraph.arc(from,through)
    val throughTo:Arc = labelDigraph.arc(through,to)

    val current:Arc = labelDigraph.arc(from,to)

    semiring.relax(fromThrough,throughTo,current)
  }

  /**
   * O(n^3)
   */
  def floydWarshall[Node,Label,Key](labelDigraph:MutableArcDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableArcDigraph[Node,Label] = {
    val innerNodes = labelDigraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      val summaryLabel = relax(labelDigraph,support.semiring)(i,k,j)
      labelDigraph.upsertArc(i,j,summaryLabel)
    }
    labelDigraph
  }

  /**
   * O(n^3)
   */
  def allPairsShortestPaths[Node,Label,Key](labelDigraph:MutableArcDigraph[Node,Label],support:SemiringSupport[Label,Key]):MutableArcDigraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   *
   * O(n ln(n) + an)
   *
   * @return a Digraph with graph's nodes, a self-arc for each node with the semiring's identifier, and an arc for each arc specified by labelForArc.
   */
  def createLabelDigraph[Node,Arc,Label,Key](arcs:Seq[(Node,Node,Arc)] = Seq.empty,
                                              extraNodes:Seq[Node] = Seq.empty,
                                              support:SemiringSupport[Label,Key],
                                              labelForArc:(Node,Node,Arc)=>Label):MutableArcDigraph[Node,Label] = {
    val nodes = (extraNodes ++ arcs.map(_._1) ++ arcs.map(_._2)).distinct
    val nonSelfArcs = arcs.filter(x => x._1 != x._2)
    val labelArcs = nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfArcs.map(x => (x._1,x._2,labelForArc(x._1,x._2,x._3)))

    import net.walend.digraph.MatrixDigraph
    MatrixDigraph(labelArcs,nodes,support.semiring.O)
  }

  /**
   * O(n^3)
   */
  def allPairsShortestPaths[Node,Arc,Label,Key](arcs:Seq[(Node,Node,Arc)] = Seq.empty,
                                                 extraNodes:Seq[Node] = Seq.empty,
                                                 support:SemiringSupport[Label,Key],
                                                 labelForArc:(Node,Node,Arc)=>Label):MutableArcDigraph[Node,Label] = {
    val initialDigraph = createLabelDigraph(arcs,extraNodes,support,labelForArc)
    floydWarshall(initialDigraph,support)
  }
}