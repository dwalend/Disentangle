package walend.scalax.gengraph

import scalax.collection.immutable.Graph
import scala.util.Random
import scalax.collection.GraphPredef._
import LDiEdge._

/**
 * Create various types of graphs.
 *
 * @author dwalend
 * @since 1/7/14 10:47 PM
 */
object GraphFactory {

  /**
   * Create a randomly connected graph, where each node has a limited number of connections to other nodes
   */
//todo ask for a common superclass of DiEdge and LDiEdge
//  def createRandomNormalGraph(nodeCount:Int,maxOutEdgesPerNode:Int):Graph[Int,DiEdge] = {
  def createRandomNormalGraph(nodeCount:Int,maxOutEdgesPerNode:Int):Graph[Int,LDiEdge] = {

    require(maxOutEdgesPerNode < nodeCount)

    val nodes:Set[Int] = (0 until nodeCount).to[Set]

//    val seqOfListOfEdges:IndexedSeq[Seq[DiEdge[Int]]] = for(fromNode:Int <- 0 until nodeCount) yield {
    val seqOfListOfEdges:IndexedSeq[Seq[LDiEdge[Int]]] = for(fromNode:Int <- 0 until nodeCount) yield {
      val toNodes:Seq[Int] = Random.shuffle((nodes - fromNode).to[Seq]).take(Random.nextInt(maxOutEdgesPerNode))
//      val someEdges:Seq[DiEdge[Int]] = for(toNode:Int <- toNodes) yield {
      val someEdges:Seq[LDiEdge[Int]] = for(toNode:Int <- toNodes) yield {
//        fromNode ~> toNode
        (fromNode ~+> toNode)("")
      }
      someEdges
    }

//    val edges:Set[DiEdge[Int]] = (for (list <- seqOfListOfEdges; x <- list) yield x).to[Set]
    val edges:Set[LDiEdge[Int]] = (for (list <- seqOfListOfEdges; x <- list) yield x).to[Set]

    Graph.from(nodes,edges)
  }

  def createFullyConnectedGraph(nodeCount:Int):Graph[Int,LDiEdge] = {
    val nodes:Set[Int] = (0 until nodeCount).to[Set]

    val edges = for(fromNode <- nodes;toNode <- nodes) yield (fromNode ~+> toNode)("")

    Graph.from(nodes,edges)
  }

}
