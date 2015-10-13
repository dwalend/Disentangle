package net.walend.disentangle.graph.semiring.benchmark

import net.walend.disentangle.graph.semiring.{Dijkstra, FewestNodes, AllPathsFirstSteps}

/**
 * @author dwalend
 * @since v0.1.2
 */
object ParDijkstraTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {
    import net.walend.disentangle.graph.DigraphFactory

    val support = new AllPathsFirstSteps[Int,Int,Int](FewestNodes)

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{Dijkstra.parAllPairsLeastPaths(graph.edges, support, support.convertEdgeToLabelFunc[Boolean](FewestNodes.convertEdgeToLabel), graph.nodes.to[Seq])}

    result._2
  }

  override def expectedTime(calibration: (Int, Long), nodeCount: Int): Long = DijkstraTiming.expectedTime(calibration,nodeCount)
}
