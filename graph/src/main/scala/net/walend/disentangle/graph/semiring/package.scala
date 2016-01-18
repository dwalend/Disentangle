package net.walend.disentangle.graph

import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps

import scala.collection.{GenSeq, GenTraversable}
import scala.collection.parallel.immutable.ParSeq

/**
 * Semirings and semiring-based graph minimizing algorithms.
 *
 * SemiringSupport is the primary trait. Algorithms in this package -- Floyd-Warshall, Dijkstra's, and Brandes'
 * algorithms -- use your choice of SemiringSupport to determine just what they are minimizing. The package also
 * includes some common SemiringSupport implementations.
 *
 * @author dwalend
 * @since v0.1.0
 */
package object semiring {


  /**
    * @since v0.2.1
    *
    * Helper methods for parsing com.typesafe.config.Config objects
    */
  implicit class LabelDigraphSemiringAlgoritms[Node,Label](self: LabelDigraph[Node,Label]) {

    def allPairsShortestPaths: Seq[(Node,Node,Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.allPairsShortestPaths(indexed.edges,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsShortestPaths(self.edges)
    }

    def parAllPairsShortestPaths: ParSeq[(Node, Node, Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.parAllPairsShortestPaths(indexed.edges,indexed.nodes.asSeq)
      case _ => Dijkstra.parAllPairsShortestPaths(self.edges)
    }

    def allPairsLeastPaths[SemiringLabel,Key](support: SemiringSupport[SemiringLabel, Key],
                                          labelForEdge: (Node, Node, Label) => SemiringLabel):Seq[(Node, Node, SemiringLabel)] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.allPairsLeastPaths(indexed.edges,support,labelForEdge,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsLeastPaths(self.edges,support,labelForEdge)
    }

    def parAllPairsLeastPaths[SemiringLabel,Key](support: SemiringSupport[SemiringLabel, Key],
                                              labelForEdge: (Node, Node, Label) => SemiringLabel):ParSeq[(Node, Node, SemiringLabel)] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.parAllPairsLeastPaths(indexed.edges,support,labelForEdge,indexed.nodes.asSeq)
      case _ => Dijkstra.parAllPairsLeastPaths(self.edges,support,labelForEdge)
    }

    def allLeastPathsAndBetweenness[CoreLabel, Key](
                                                     coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                     labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Brandes.allLeastPathsAndBetweenness(indexed.edges,indexed.nodes.asSeq,coreSupport,labelForEdge)
      case _ => Brandes.allLeastPathsAndBetweenness(self.edges,coreSupport = coreSupport,labelForEdge = labelForEdge)
    }

    def parAllLeastPathsAndBetweenness[CoreLabel, Key](
                                                     coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                     labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Brandes.allLeastPathsAndBetweenness(indexed.edges,indexed.nodes.asSeq,coreSupport,labelForEdge)
      case _ => Brandes.allLeastPathsAndBetweenness(self.edges,coreSupport = coreSupport,labelForEdge = labelForEdge)
    }

    //todo Brandes test , same for Undirected Graphs with a special version of Brandes
  }

}
//todo pathcount as a decorator semiring
//todo core-and-many-decorator semiring
//todo flag in SemiringSupport for where Dijkstra's algorithm is OK