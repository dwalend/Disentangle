package net.walend.graph.semiring.benchmark

import net.walend.graph.semiring.{Brandes, FewestNodes}

/**
 * @author dwalend
 * @since v0.0.1
 */
object BrandesTiming {

  def main (args:Array[String]) {

    val maxExponent = if (args.size == 0) 7
    else java.lang.Integer.parseInt(args(1))

    //Time the algorithm with AllShortestPaths
    val results = createResults(maxExponent)
    results.map(x => println(x))
  }

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
