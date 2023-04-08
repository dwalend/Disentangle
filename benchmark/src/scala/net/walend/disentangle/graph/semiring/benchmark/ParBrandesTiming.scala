package net.walend.disentangle.graph.semiring.benchmark

import net.walend.disentangle.graph.semiring.Brandes
import net.walend.disentangle.graph.semiring.FewestNodes

import net.walend.disentangle.graph.semiring.par.ParBrandes

/**
 *
 *
 * @author dwalend
 * @since v0.1.2
 */

object ParBrandesTiming extends Timeable {

  def measureTime(nodeCount:Int):Long = {
    import net.walend.disentangle.graph.DigraphFactory

    val support = FewestNodes

    val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

    val result = TimingStudy.timeFunction{ParBrandes.parAllLeastPathsAndBetweenness(Seq.from(graph.edges),Seq.from(graph.nodes),support,FewestNodes.convertEdgeToLabel)}

    result._2
  }

  def expectedTime(calibration:(Int,Long),nodeCount:Int):Long = {
    DijkstraTiming.expectedTime(calibration,nodeCount)
  }
}


