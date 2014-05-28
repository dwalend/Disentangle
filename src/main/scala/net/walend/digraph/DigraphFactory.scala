package net.walend.digraph

import scala.util.Random

/**
 * Create various types of graphs.
 *
 * @author dwalend
 * @since v0.1.0
 */
object DigraphFactory {

  /**
   * Create a randomly connected graph, where each node has a limited number of connections to other nodes
   */
  def createRandomNormalDigraph(nodeCount:Int,maxOutEdgesPerNode:Int):Digraph[Int,Boolean] = {

    require(maxOutEdgesPerNode < nodeCount)

    val nodes:Set[Int] = (0 until nodeCount).to[Set]

    val seqOfListOfEdges:IndexedSeq[Seq[(Int,Int,Boolean)]] = for(fromNode:Int <- 0 until nodeCount) yield {
      val toNodes:Seq[Int] = Random.shuffle((nodes - fromNode).to[Seq]).take(Random.nextInt(maxOutEdgesPerNode))
      val someEdges:Seq[(Int,Int,Boolean)] = for(toNode:Int <- toNodes) yield {
        (fromNode,toNode,true)
      }
      someEdges
    }

    val edges:Seq[(Int,Int,Boolean)] = seqOfListOfEdges.flatten

    AdjacencyDigraph(edges,nodes.to[Seq],false)
  }
}
