package net.walend.scalagraph.minimizer.gengraph

import scalax.collection.immutable.Graph
import scala.util.Random
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge

/**
 * Create various types of graphs.
 *
 * @author dwalend
 * @since v0.0.0
 */
object GraphFactory {

  /**
   * Create a randomly connected graph, where each node has a limited number of connections to other nodes
   */
  def createRandomNormalGraph(nodeCount:Int,maxOutEdgesPerNode:Int):Graph[Int,DiEdge] = {

    require(maxOutEdgesPerNode < nodeCount)

    val nodes:Set[Int] = (0 until nodeCount).to[Set]

    val seqOfListOfEdges:IndexedSeq[Seq[DiEdge[Int]]] = for(fromNode:Int <- 0 until nodeCount) yield {
      val toNodes:Seq[Int] = Random.shuffle((nodes - fromNode).to[Seq]).take(Random.nextInt(maxOutEdgesPerNode))
      val someEdges:Seq[DiEdge[Int]] = for(toNode:Int <- toNodes) yield {
        fromNode ~> toNode
      }
      someEdges
    }

    val edges:Set[DiEdge[Int]] = (for (list <- seqOfListOfEdges; x <- list) yield x).to[Set]

    Graph.from(nodes,edges)
  }


}
