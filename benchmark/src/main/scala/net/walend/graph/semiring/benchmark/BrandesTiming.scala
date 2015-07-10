package net.walend.graph.semiring.benchmark

import net.walend.graph.semiring.{Brandes, FewestNodes}

/**
 * @author dwalend
 * @since v0.0.1
 */
object BrandesTiming extends TimingStudy {

  def createResults(maxExponent:Int) = {
    TimingStudy.study(maxExponent,timeBrandes,DijkstraTiming.expectedTimeDijkstra)
  }

  def timeBrandes(nodeCount:Int):Long = {

    import net.walend.graph.DigraphFactory

    val support = FewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{Brandes.allLeastPathsAndBetweenness(graph.edges,graph.nodes.to[Seq],support,FewestNodes.convertEdgeToLabel)}

    result._2
  }
}
