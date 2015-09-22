package net.walend.graph.semiring.benchmark

/**
 * @author dwalend
 * @since v0.1.2
 */
object ParDijkstraTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {

    import net.walend.graph.DigraphFactory
    import net.walend.graph.semiring.{AllPathsFirstSteps, Dijkstra => DDijkstra, FewestNodes => FFewestNodes}

    val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{DDijkstra.parAllPairsShortestPaths(graph.edges,graph.nodes.to[Seq],support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))}

    result._2
  }

  override def expectedTime(calibration: (Int, Long), nodeCount: Int): Long = DijkstraTiming.expectedTime(calibration,nodeCount)
}
