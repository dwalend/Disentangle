package net.walend.disentangle.graph.semiring.benchmark

import net.walend.disentangle.graph.DigraphFactory
import net.walend.disentangle.graph.semiring.{FewestNodes, FloydWarshall, AllPathsFirstSteps}

/**
 * @author dwalend
 * @since v0.0.1
 */
object FloydWarshallTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {

    val support = new AllPathsFirstSteps[Int,Int,Int](FewestNodes)

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{FloydWarshall.allPairsLeastPaths(graph.edges,Seq.from(graph.nodes),support,support.convertEdgeToLabelFunc[Boolean](FewestNodes.convertEdgeToLabel))}

    result._2
  }

  def expectedTime(calibration:(Int,Long),nodeCount:Int):Long = {
    (Math.pow(nodeCount.toDouble/calibration._1,3) * calibration._2).toLong
  }
}
