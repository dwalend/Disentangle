package net.walend.disentangle.graph.semiring.par

import net.walend.disentangle.graph.IndexedLabelDigraph
import net.walend.disentangle.graph.semiring.Brandes.{BrandesSteps, BrandesSupport}
import net.walend.disentangle.graph.semiring.{Brandes, FewestNodes, SemiringSupport}
import net.walend.disentangle.heap.HeapOrdering

import scala.collection.immutable.Iterable

import scala.collection.parallel.immutable.{ParMap, ParSeq}


/**
 * Brandes' algorithm for betweenness and minimal paths.
 *
 * @author dwalend
 * @since v0.1.0
 */
//noinspection ReferenceMustBePrefixed

object ParBrandes {

  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def parAllLeastPathsAndBetweenness[Node, EdgeLabel, CoreLabel, Key](
                                                                       edges: Iterable[(Node, Node, EdgeLabel)],
                                                                       nodeOrder: Seq[Node] = Seq.empty,
                                                                       coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                                       labelForEdge: (Node, Node, EdgeLabel) => CoreLabel = FewestNodes.edgeToLabelConverter
                                                                     ):
  (ParSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], ParMap[Node, Double]) = {
    val support = new BrandesSupport[Node,CoreLabel,Key](coreSupport)
    type Label = support.Label

    //todo make parallel
    val initialGraph: IndexedLabelDigraph[Node,Label] = Brandes.createLabelDigraph(edges, nodeOrder, support, labelForEdge)

    val innerNodes = ParSeq.fromSpecific(initialGraph.innerNodes)

    val edgesAndBetweenParts = for (sink <- innerNodes) yield {
      val edgeAndNodeStack = Brandes.brandesDijkstra(initialGraph, support)(sink)
      val partialB = Brandes.partialBetweenness(support, initialGraph)(sink, edgeAndNodeStack._2, edgeAndNodeStack._1)
      val filteredEdges = edgeAndNodeStack._1.filter(_._3 != support.semiring.O)
      (filteredEdges, partialB)
    }

    def betweennessForNode(innerNode: initialGraph.InnerNodeType): Double = edgesAndBetweenParts.map(x => x._2(innerNode.index)).sum

    //noinspection ScalaUnnecessaryParentheses
    val betweennessMap = innerNodes.map(innerNode => (innerNode.value -> betweennessForNode(innerNode))).toMap

    val shortPaths = edgesAndBetweenParts.flatMap(x => x._1)

    (shortPaths, betweennessMap)
  }
}