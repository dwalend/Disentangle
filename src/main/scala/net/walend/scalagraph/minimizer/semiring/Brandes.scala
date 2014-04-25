package net.walend.scalagraph.minimizer.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import net.walend.scalagraph.minimizer.heap.{FibonacciHeap, Heap}
import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag
import scalax.collection.GraphPredef._
import scala.Some

/**
 * Brandes' algorithm for betweenness.
 *
 * @author dwalend
 * @since v1
 */
object Brandes {

  //todo do you really need the TypeTags ?
  def partialBetweenness[N: TypeTag, CL, Label <: Option[Steps[N, CL]], Key](support: AllPaths[N, CL, Key])
                                                                                      (labelGraph: Graph[N, MLDiEdge])
                                                                                      (sink: labelGraph.NodeT): Map[N, Double] = {

    //Get all the successor edges, and sort them using the heap comparator, farthest to closest
    class EdgeOrdering extends Ordering[labelGraph.EdgeT] {

      def keyForEdge(edge: labelGraph.EdgeT): Key = {
        val label: Label = edge.label.asInstanceOf[Label]
        support.heapKeyForLabel(label)
      }

      //todo test this. Might need to be the opposite order
      override def compare(x: labelGraph.type#EdgeT, y: labelGraph.type#EdgeT): Int = {
        val xKey = keyForEdge(x)
        val yKey = keyForEdge(y)
        if (support.heapOrdering.lt(xKey, yKey)) -1
        else if (support.heapOrdering.gt(xKey, yKey)) 1
        else 0
      }
    }
    //todo do a variation that takes the sort in from Dijkstra's algorithm building a stack
    val edges: Seq[labelGraph.EdgeT] = sink.incoming.to[Seq].sorted(new EdgeOrdering)

    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[N, Double] = MutableMap()

    //for each possible choice of next step
    for (edge: labelGraph.EdgeT <- edges) {
      //figure out the partial betweenness to apply to that step
      val label: Label = edge.label.asInstanceOf[Label]
      label match {
        case None => //should never happen, but do nothing if it does
        case Some(sourceLabel: Steps[N, CL]) => {
          val numChoices: Double = sourceLabel.choices.size
          val partialFromSource = nodesToPartialBetweenness.getOrElse(edge._1.value, 0.0)
          for (choice: N <- sourceLabel.choices) {
            //only calculate betweenness for the between nodes, not arriving at the sink
            if (choice != sink.value)  {
              val oldPartial: Double = nodesToPartialBetweenness.getOrElse(choice, 0)
              //new value is the old value plus (value for coming through the source, plus the source)/number of choices
              val newPartial: Double = oldPartial + ((1.0 + partialFromSource) / numChoices)
              nodesToPartialBetweenness.put(choice, newPartial)
            }
          }
        }
      }
    }

    nodesToPartialBetweenness.toMap
  }

  /**
   * This method finds betweenness for all nodes.
   */
  def betweenness[N: TypeTag, CL, Label <: Option[Steps[N, CL]], Key](support: AllPaths[N, CL, Key])
                                                                               (labelGraph: Graph[N, MLDiEdge]): Map[N, Double] = {
    val partialBetweennesses: Seq[Map[N, Double]] = for (node: labelGraph.NodeT <- labelGraph.nodes.to[Seq]) yield {
      partialBetweenness(support)(labelGraph)(node)
    }

    def betweennessForNode(node: N): Double = {
      partialBetweennesses.map(x => x.getOrElse(node, 0.0)).sum
    }
    labelGraph.nodes.map(node => (node.value, betweennessForNode(node))).toMap
  }

  /**
   * This method runs Dijkstra's algorithm and finds betweenness for all nodes in the label graph.
   */
  def allLeastPathsAndBetweenness[N:Manifest,
                                  CL,
                                  Label <: Option[Steps[N, CL]],
                                  Key]
                                  (support:AllPaths[N, CL, Key])
                                  (originalLabelGraph:MutableGraph[N,MLDiEdge]):(Graph[N,MLDiEdge],Map[N, Double]) = {

    val labelGraph: Graph[N, MLDiEdge] = Dijkstra.allPairsShortestPaths(support)(originalLabelGraph)
    val betweennessMap = betweenness(support)(labelGraph)
    (labelGraph,betweennessMap)
  }

  /**
   * This method creates the label graph, runs Dijkstra's algorithm for all nodes, and calculates betweenness.
   */
  def allLeastPathsAndBetweenness[N:Manifest,
                                  CL,
                                  E[X] <: EdgeLikeIn[X],
                                  Label <: Option[Steps[N, CL]],
                                  Key]
                                  (support:AllPaths[N, CL, Key],labelGraphBuilder:LabelGraphBuilder[N,Label])
                                  (originalGraph:Graph[N,E]):(Graph[N,MLDiEdge],Map[N, Double]) = {

    val startingLabelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)
    allLeastPathsAndBetweenness(support)(startingLabelGraph)
  }

}