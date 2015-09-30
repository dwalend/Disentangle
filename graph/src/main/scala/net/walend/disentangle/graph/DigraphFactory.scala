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
    val nodes:Set[Int] = (0 until nodeCount).to[Set]

    val seqOfListOfEdges = for(fromNode:Int <- 0 until nodeCount) yield {
      val toNodes:Seq[Int] = Random.shuffle((nodes - fromNode).to[Seq]).take(Random.nextInt(maxOutEdgesPerNode))
//      val toNodes:Set[Int] = shuffleAndTake(nodes,Random.nextInt(maxOutEdgesPerNode),fromNode)
      val someEdges = for(toNode:Int <- toNodes) yield {
        (fromNode,toNode,true)
      }
      someEdges
    }

    val edges:Seq[(Int,Int,Boolean)] = seqOfListOfEdges.flatten

    AdjacencyLabelDigraph(edges,nodes.to[Seq],false)
  }

  import scala.collection.mutable.{Set => MutableSet}
  private def shuffleAndTake[T](items:Seq[T],toTake:Int,never:T):Set[T] = {
    val result:MutableSet[T] = scala.collection.mutable.Set()
    while(result.size < toTake) {
      val toAdd = Random.nextInt(items.size)
      if(toAdd != never) result.add(items(Random.nextInt(items.size)))
    }
    result.to[Set]
  }

}