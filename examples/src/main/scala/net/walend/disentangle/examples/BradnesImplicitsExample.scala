package net.walend.disentangle.examples

import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps
import net.walend.disentangle.graph.semiring.LabelUndigraphSemiringAlgorithms
import net.walend.disentangle.graph.{AdjacencyLabelUndigraph, NodePair}

/**
  * Use Brandes' algorithms to find least paths and betweenness for a directed graph.
  *
  * @author dwalend
  * @since v0.2.1
  */
object BrandesImplicitsExample {

  /**
    * Edges are just a Seq of Tuple3[Node,Node,Edge]
    */
  lazy val edges: Seq[(NodePair[String], String)] = Seq(
    (NodePair("A","B"),"ab"),
    (NodePair("B","C"),"bc"),
    (NodePair("C","D"),"cd"),
    (NodePair("D","E"),"de"),
    (NodePair("E","F"),"ef"),
    (NodePair("E","B"),"eb"),
    (NodePair("E","H"),"eh"),
    (NodePair("H","C"),"hc")
  )

  /**
    * The labels from Brandes use node indexes from a directed graph, so it's best to control those via the optional nodeOrder parameter
    */
  lazy val nodeOrder = Array("A","B","C","D","E","F","H")

  val graph = AdjacencyLabelUndigraph(edges,nodeOrder)

  lazy val brandesResults = graph.allLeastPathsAndBetweenness()

  lazy val nextStepsAndCosts: IndexedSeq[(String, String, Option[BrandesSteps[String, Int]])] = brandesResults._1

  lazy val betweennessValues: Map[String, Double] = brandesResults._2

}
