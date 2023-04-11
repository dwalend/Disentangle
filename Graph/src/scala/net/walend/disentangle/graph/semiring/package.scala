package net.walend.disentangle.graph

import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps

import scala.collection.immutable.Iterable

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
    * Helper methods for LabelDigraphs
    */
  implicit class LabelDigraphSemiringAlgorithms[Node,Label](self: LabelDigraph[Node,Label]) {

    def allPairsShortestPaths: Seq[(Node,Node,Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.allPairsShortestPaths(indexed.edges,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsShortestPaths(self.edges)
    }

    def allPairsLeastPaths[SemiringLabel,Key](support: SemiringSupport[SemiringLabel, Key],
                                          labelForEdge: (Node, Node, Label) => SemiringLabel):Seq[(Node, Node, SemiringLabel)] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.allPairsLeastPaths(indexed.edges,support,labelForEdge,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsLeastPaths(self.edges,support,labelForEdge)
    }

    def allLeastPathsAndBetweenness[CoreLabel, Key]( coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                     labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Brandes.allLeastPathsAndBetweenness(indexed.edges,indexed.nodes.asSeq,coreSupport,labelForEdge)
      case _ => Brandes.allLeastPathsAndBetweenness(self.edges,coreSupport = coreSupport,labelForEdge = labelForEdge)
    }
  }

  /**
    * @since v0.2.1
    *
    * Helper methods for LabelUndigraphs
    */
  implicit class LabelUndigraphSemiringAlgorithms[Node,Label](self: LabelUndigraph[Node,Label]) {

    def allPairsShortestPaths: Seq[(Node,Node,Option[FirstStepsTrait[Node, Int]])] = self match {
      case indexed:IndexedLabelUndigraph[Node,Label] => Dijkstra.allPairsShortestPaths(diEdges,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsShortestPaths(diEdges)
    }

    def allPairsLeastPaths[SemiringLabel,Key](support: SemiringSupport[SemiringLabel, Key],
                                              labelForEdge: (Node, Node, Label) => SemiringLabel):Seq[(Node, Node, SemiringLabel)] = self match {
      case indexed:IndexedLabelDigraph[Node,Label] => Dijkstra.allPairsLeastPaths(diEdges,support,labelForEdge,indexed.nodes.asSeq)
      case _ => Dijkstra.allPairsLeastPaths(diEdges,support,labelForEdge)
    }

    def allLeastPathsAndBetweenness[CoreLabel, Key]( coreSupport: SemiringSupport[CoreLabel, Key] = FewestNodes,
                                                     labelForEdge: (Node, Node, Label) => CoreLabel = FewestNodes.edgeToLabelConverter): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {
      val digraphResult = self match {
        case indexed:IndexedLabelDigraph[Node,Label] => Brandes.allLeastPathsAndBetweenness(indexed.edges,indexed.nodes.asSeq,coreSupport,labelForEdge)
        case _ => Brandes.allLeastPathsAndBetweenness(diEdges,coreSupport = coreSupport,labelForEdge = labelForEdge)
      }
      correctForUndigraph(digraphResult)
    }

    def diEdges: Iterable[(Node, Node, Label)] = {
      self.edges.map(e => (e._1._1,e._1._2,e._2)) ++ self.edges.map(e => (e._1._2,e._1._1,e._2))
    }

    def correctForUndigraph[CoreLabel](
                                        digraphResult: (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double])
                                      ): (IndexedSeq[(Node, Node, Option[BrandesSteps[Node, CoreLabel]])], Map[Node, Double]) = {
      val halfMap = digraphResult._2.map(x => (x._1,x._2/2))
      (digraphResult._1,halfMap)
    }

  }
  //todo add for Digraphs and Undirected graphs without labels
}
//todo pathcount as a decorator semiring
//todo core-and-many-decorator semiring
//todo flag in SemiringSupport for where Dijkstra's algorithm is OK