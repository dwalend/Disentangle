package net.walend.disentangle.graph

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
  def createRandomNormalDigraph(nodeCount:Int,maxOutEdgesPerNode:Int):IndexedLabelDigraph[Int,Boolean] = {

    require(maxOutEdgesPerNode < nodeCount)

//    val nodes:Range = 0 until nodeCount
    val nodes:Seq[Int] = 0 until nodeCount

    //todo nodes.par
    val seqOfListOfEdges = nodes.map{fromNode =>
      shuffleAndTake(Set.from(nodes),Random.nextInt(maxOutEdgesPerNode),fromNode).map(toNode => (fromNode,toNode,true))
    }

    val edges:Seq[(Int,Int,Boolean)] = Seq.from(seqOfListOfEdges.flatten)

    AdjacencyLabelDigraph(edges,nodes,false)
  }

  def shuffleAndTake[T](items:Set[T],toTake:Int,never:T):Seq[T] = {
    Random.shuffle(Seq.from(items - never)).take(toTake)
  }
}
