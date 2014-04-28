package net.walend.scalagraph.minimizer.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import net.walend.scalagraph.minimizer.heap.{FibonacciHeap, Heap}
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

  /**
   * Dijkstra's algorithm, run backwards.
   */
  def dijkstraToSink [N:Manifest,Label,Key](labelGraph:MutableGraph[N,MLDiEdge])
                                     (sink:labelGraph.NodeT)
                                     (support:GraphMinimizerSupport[Label,Key]):(Graph[N,MLDiEdge],Seq[labelGraph.EdgeT]) = {
    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.NodeT] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.NodeT,heap.HeapMember] = labelGraph.nodes.map(node => (node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node)))(breakOut)

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(sink,throw new IllegalStateException("No HeapMember for sink "+sink)).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //Stack of visited nodes
    import scala.collection.mutable.Stack
    val stack:Stack[labelGraph.EdgeT] = Stack[labelGraph.EdgeT]()

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      topNode ~>? sink match {
        case Some(edge) => stack.push(edge)
        case None => //ignore
      }

      //For any node that this node can reach and is not yet visited (because it's key is still in the heap)
      for(predecessor <- topNode.diPredecessors) {
        //if the node has not yet been visited (because its key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(predecessor,throw new IllegalStateException("No HeapMember for "+predecessor))
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = support.semiring.relax(labelGraph)(predecessor,topNode,sink)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    (labelGraph,stack)
  }

  //todo do you really need the TypeTags ?
  /**
   * Find partial betweenness
   */
  def partialBetweenness[N: TypeTag, CL, Label <: Option[Steps[N, CL]], Key](support: AllPaths[N, CL, Key])
                                                                            (labelGraph: Graph[N, MLDiEdge])
                                                                            (sink: labelGraph.NodeT,edges: Seq[labelGraph.EdgeT]): Map[N, Double] = {

    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness: MutableMap[N, Double] = MutableMap()

    //for each possible choice of next step
    for (edge <- edges) {
      //figure out the partial betweenness to apply to that step
      val label: Label = edge.label.asInstanceOf[Label]
      label match {
        case None => //should never happen, but do nothing if it does
        case Some(sourceLabel: Steps[N, CL]) => {
          val numChoices: Double = sourceLabel.choices.size
          val partialFromSource = nodesToPartialBetweenness.getOrElse(edge._1.value, 0.0)
          for (choice <- sourceLabel.choices) {
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
   * Sorts internally, for when you don't get a stack from Dijkstra's algorithm or some other side effect.
   *
   */
  def partialBetweennessWithSort[N: TypeTag, CL, Label <: Option[Steps[N, CL]], Key](support: AllPaths[N, CL, Key])
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

    partialBetweenness(support)(labelGraph)(sink,edges)
  }

  /**
   * This method finds betweenness for all nodes, given a labelGraph with all least paths.
   */
  def betweenness[N: TypeTag, CL, Label <: Option[Steps[N, CL]], Key](support: AllPaths[N, CL, Key])
                                                                               (labelGraph: Graph[N, MLDiEdge]): Map[N, Double] = {
    val partialBetweennesses: Seq[Map[N, Double]] = for (node <- labelGraph.nodes.to[Seq]) yield {
      partialBetweennessWithSort(support)(labelGraph)(node)
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
                                  (labelGraph:MutableGraph[N,MLDiEdge]):(Graph[N,MLDiEdge],Map[N, Double]) = {

    val partialBetweennesses: Seq[Map[N, Double]] = for(sink <- labelGraph.nodes.to[Seq]) yield {
      val labelGraphAndNodeStack = dijkstraToSink(labelGraph)(sink)(support)
      partialBetweenness(support)(labelGraph)(sink,labelGraphAndNodeStack._2)
    }

    def betweennessForNode(node: N): Double = {
      partialBetweennesses.map(x => x.getOrElse(node, 0.0)).sum
    }
    val betweennessMap:Map[N, Double] = labelGraph.nodes.map(node => (node.value, betweennessForNode(node))).toMap

    (labelGraph,betweennessMap)
  }

  /**
   * This method creates the label graph, runs Dijkstra's algorithm and calculates betweenness for all nodes.
   */
  import scala.language.higherKinds
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