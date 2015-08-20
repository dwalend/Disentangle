package net.walend.graph.semiring.benchmark

import net.walend.graph.semiring.FloydWarshall
import net.walend.graph.DigraphFactory
import net.walend.graph.semiring.{AllPathsFirstSteps, FewestNodes => FFewestNodes}

/**
 * @author dwalend
 * @since v0.0.1
 */
object FloydWarshallTiming extends TimingStudy {

  def createResults(minExponent:Int,maxExponent:Int):Seq[(Int,Long,Long,Double)] = {
    TimingStudy.study(minExponent,maxExponent,timeFloyd,expectedTimeFloyd)
  }

  def timeFloyd(nodeCount:Int):Long = {

    val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{FloydWarshall.allPairsShortestPaths(graph.edges,graph.nodes.to[Seq],support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))}

    result._2
  }

  def expectedTimeFloyd(calibration:(Int,Long),nodeCount:Int):Long = {
    (Math.pow(nodeCount.toDouble/calibration._1,3) * calibration._2).toLong
  }
}
