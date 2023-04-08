package net.walend.disentangle.graph.semiring.benchmark

import net.walend.disentangle.graph.semiring.{AllPathsFirstSteps, Dijkstra, FewestNodes}

import net.walend.disentangle.graph.semiring.par.ParDijkstra

/**
 * @author dwalend
 * @since v0.1.2
 */

object ParDijkstraTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {
    import net.walend.disentangle.graph.DigraphFactory

    val support = new AllPathsFirstSteps[Int,Int,Int](FewestNodes)

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{ParDijkstra.parAllPairsLeastPaths(graph.edges, support, support.convertEdgeToLabelFunc[Boolean](FewestNodes.convertEdgeToLabel), Seq.from(graph.nodes))}

    result._2
  }

  override def expectedTime(calibration: (Int, Long), nodeCount: Int): Long = DijkstraTiming.expectedTime(calibration,nodeCount)
}
