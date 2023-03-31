package net.walend.disentangle.graph.semiring.benchmark

import net.walend.disentangle.graph.semiring.{Brandes, FewestNodes}

/**
 * @author dwalend
 * @since v0.0.1
 */
object BrandesTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {
    import net.walend.disentangle.graph.DigraphFactory

    val support = FewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{Brandes.allLeastPathsAndBetweenness(graph.edges,Seq.from(graph.nodes),support,FewestNodes.convertEdgeToLabel)}

    result._2
  }

  def expectedTime(calibration:(Int,Long),nodeCount:Int):Long = {
    DijkstraTiming.expectedTime(calibration,nodeCount)
  }
}
