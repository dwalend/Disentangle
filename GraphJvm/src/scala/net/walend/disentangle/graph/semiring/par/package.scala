package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps
import net.walend.disentangle.graph.{IndexedLabelDigraph, IndexedLabelUndigraph, LabelDigraph, LabelUndigraph}
import net.walend.disentangle.graph.semiring.{Dijkstra, FewestNodes, FirstStepsTrait, SemiringSupport}

import scala.collection.immutable.Iterable

import scala.collection.parallel.immutable.{ParMap, ParSeq}

package object par {
  /**
   * @since v0.2.1
   *
   *        Helper methods for LabelDigraphs
   */
  implicit class LabelDigraphSemiringAlgorithms[Node, Label](self: LabelDigraph[Node, Label]) {

    def parAllPairsShortestPaths: ParSeq[(Node, Node, Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed: IndexedLabelDigraph[Node, Label] => ParDijkstra.parAllPairsShortestPaths(indexed.edges, indexed.nodes.asSeq)
      case _ => ParDijkstra.parAllPairsShortestPaths(self.edges)
    }

    def parAllPairsLeastPaths[SemiringLabel, Key](support: SemiringSupport[SemiringLabel, Key],
                                                  labelForEdge: (Node, Node, Label) => SemiringLabel): ParSeq[(Node, Node, SemiringLabel)] = self match {
      case indexed: IndexedLabelDigraph[Node, Label] => ParDijkstra.parAllPairsLeastPaths(indexed.edges, support, labelForEdge, indexed.nodes.asSeq)
      case _ => ParDijkstra.parAllPairsLeastPaths(self.edges, support, labelForEdge)
    }

    def parAllLeastPathsAndBetweenness[CoreLabel, Key](coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                       labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter): (ParSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], ParMap[Node, Double]) = self match {
      case indexed: IndexedLabelDigraph[Node, Label] => ParBrandes.parAllLeastPathsAndBetweenness(indexed.edges, indexed.nodes.asSeq, coreSupport, labelForEdge)
      case _ => ParBrandes.parAllLeastPathsAndBetweenness(self.edges, coreSupport = coreSupport, labelForEdge = labelForEdge)
    }
  }

  /**
   * @since v0.2.1
   *
   *        Helper methods for LabelUndigraphs
   */
  implicit class LabelUndigraphSemiringAlgorithms[Node, Label](self: LabelUndigraph[Node, Label]) {

    def parAllPairsShortestPaths: ParSeq[(Node, Node, Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed: IndexedLabelUndigraph[Node, Label] => ParDijkstra.parAllPairsShortestPaths(diEdges, indexed.nodes.asSeq)
      case _ => ParDijkstra.parAllPairsShortestPaths(diEdges)
    }

    def parAllPairsLeastPaths[SemiringLabel, Key](support: SemiringSupport[SemiringLabel, Key],
                                                  labelForEdge: (Node, Node, Label) => SemiringLabel): ParSeq[(Node, Node, SemiringLabel)] = self match {
      case indexed: IndexedLabelDigraph[Node, Label] => ParDijkstra.parAllPairsLeastPaths(diEdges, support, labelForEdge, indexed.nodes.asSeq)
      case _ => ParDijkstra.parAllPairsLeastPaths(diEdges, support, labelForEdge)
    }

    def parAllLeastPathsAndBetweenness[CoreLabel, Key](
                                                        coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                        labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter
                                                      ): (ParSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], ParMap[Node, Double]) = {
      val digraphResult = self match {
        case indexed: IndexedLabelDigraph[Node, Label] => ParBrandes.parAllLeastPathsAndBetweenness(indexed.edges, indexed.nodes.asSeq, coreSupport, labelForEdge)
        case _ => ParBrandes.parAllLeastPathsAndBetweenness(diEdges, coreSupport = coreSupport, labelForEdge = labelForEdge)
      }
      correctForUndigraph(digraphResult)
    }

    def diEdges: Iterable[(Node, Node, Label)] = {
      self.edges.map(e => (e._1._1, e._1._2, e._2)) ++ self.edges.map(e => (e._1._2, e._1._1, e._2))
    }

    def correctForUndigraph[CoreLabel](
                                        digraphResult: (ParSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], ParMap[Node, Double])
                                      ) = {
      val halfMap = digraphResult._2.map(x => (x._1, x._2 / 2))
      (digraphResult._1, halfMap)
    }

  }
}
