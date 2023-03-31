package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.mutable.{MutableLabelDigraph, MatrixLabelDigraph}
import net.walend.disentangle.graph.IndexedLabelDigraph
import scala.collection.{Seq, Iterable}

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
  def floydWarshall[Node,Label,Key](labelDigraph:MatrixLabelDigraph[Node,Label],support:SemiringSupport[Label,Key]):IndexedLabelDigraph[Node,Label] = {
    val innerNodes = labelDigraph.innerNodes
    for (k <- innerNodes; i <- innerNodes; j <- innerNodes) {
      val summaryLabel = relax(labelDigraph,support.semiring)(i,k,j)
      labelDigraph.upsertEdge(i,j,summaryLabel)
    }
    labelDigraph
  }

  /**
   * O(n&#94;3)
   */
  def allPairsLeastPaths[Node,Label,Key](labelDigraph:MatrixLabelDigraph[Node,Label],support:SemiringSupport[Label,Key]):IndexedLabelDigraph[Node,Label] = {

    floydWarshall(labelDigraph,support)
  }

  /**
   * Create a digraph of Labels from an arbitrary Digraph.
   *
   * O(n ln(n) + an)
   *
   * @return a Digraph with graph's nodes, a self-edge for each node with the semiring's identifier, and an edge for each edge specified by labelForEdge.
   */
  def createLabelDigraph[Node,EdgeLabel,Label,Key](edges:Iterable[(Node,Node,EdgeLabel)] = Seq.empty,
                                              extraNodes:Seq[Node] = Seq.empty,
                                              support:SemiringSupport[Label,Key],
                                              labelForEdge:(Node,Node,EdgeLabel)=>Label):MatrixLabelDigraph[Node,Label] = {
    val nodes = (extraNodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct
    val nonSelfEdges = edges.filter(x => x._1 != x._2)
    val labelEdges = nodes.map(x => (x,x,support.semiring.I)) ++
      nonSelfEdges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    MatrixLabelDigraph(labelEdges,nodes,support.semiring.O)
  }

  /**
   * O(n&#94;3)
   */
  def allPairsLeastPaths[Node,EdgeLabel,Label,Key](edges:Iterable[(Node,Node,EdgeLabel)] = Seq.empty,
                                                 extraNodes:Seq[Node] = Seq.empty,
                                                 support:SemiringSupport[Label,Key],
                                                 labelForEdge:(Node,Node,EdgeLabel)=>Label):IndexedLabelDigraph[Node,Label] = {
    val initialDigraph = createLabelDigraph(edges,extraNodes,support,labelForEdge)
    floydWarshall(initialDigraph,support)
  }

  def defaultSupport[Node] = AllPathsFirstSteps[Node,Int,Int](FewestNodes)

  def allPairsShortestPaths[Node,EdgeLabel](edges:Iterable[(Node,Node,EdgeLabel)],
                                            nodeOrder:Seq[Node] = Seq.empty
                                           ):IndexedLabelDigraph[Node,Option[FirstStepsTrait[Node, Int]]] = {
    val support = defaultSupport[Node]
    allPairsLeastPaths(edges, nodeOrder, support, support.convertEdgeToLabel(FewestNodes.convertEdgeToLabel))
  }

}