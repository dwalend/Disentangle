package walend.scalax.semiring

import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import walend.scalax.heap.{FibonacciHeap, Heap}
import scala.collection.mutable
import scala.reflect.ClassTag
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
   * Brandes' algorithm.
   */
  def brandesForSource[N:Manifest,Label <: Option[BrandesLabel[N]]:ClassTag,Key](labelGraph:MutableGraph[N,MLDiEdge])
                                     (source:labelGraph.NodeT)
                                     (support:GraphMinimizerSupport[Label,Key]):(Graph[N,MLDiEdge],Map[labelGraph.NodeT,Double]) = {

    //Set up the map of Nodes to HeapKeys
    val heap:Heap[Key,labelGraph.NodeT] = new FibonacciHeap(support.heapOrdering)
    import scala.collection.breakOut
    val nodesToHeapMembers:Map[labelGraph.NodeT,heap.HeapMember] = labelGraph.nodes.map(node => (node -> heap.insert(support.heapKeyForLabel(support.semiring.O),node)))(breakOut)

    //Stack to use later
    val stack:mutable.Stack[labelGraph.NodeT] = mutable.Stack()

    //Raise innerSourceNode's to I
    nodesToHeapMembers.getOrElse(source,throw new IllegalStateException("No HeapMember for source "+source)).raiseKey(support.heapKeyForLabel(support.semiring.I))

    //While the heap is not empty
    while(!heap.isEmpty) {
      //take the top node
      val topNode = heap.takeTopValue()
      //add the top node to the stack
      stack.push(topNode)
      //For any node that is reachable from this node not yet visited (because its key is still in the heap)
      for(successor <- topNode.diSuccessors) {
        //if the node has not yet been visited (because it's key is still in the heap)
        val heapKey = nodesToHeapMembers.getOrElse(successor,throw new IllegalStateException("No HeapMember for "+successor))
        if(heapKey.isInHeap) {
          //Relax to get a new label
          val label = support.semiring.relax(labelGraph)(source,topNode,successor)
          //Try to change the key
          heapKey.raiseKey(support.heapKeyForLabel(label))
        }
      }
    }

    //todo this seems like it could use a list, an accumulator, and tail recursion
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness:MutableMap[labelGraph.NodeT,Double] = MutableMap()
    while(!stack.isEmpty) {
      val sink:labelGraph.NodeT = stack.pop()
      //don't bother finding the partial for the sink
      if (sink != source) {
        getLabelBetween[N,Label](labelGraph)(source,sink) match {
          case None =>
          case Some(sinkLabel) => {

            for(outerPredecessor <- sinkLabel.predecessors) {

              val predecessor = labelGraph get outerPredecessor
              //add to its partial weight (1 for this sink plus the partial weight accumulated for this sink)*(number of this predecessor's predecessors/number of predecessors)
              val oldPartial:Double = nodesToPartialBetweenness.getOrElse(predecessor,0)
              getLabelBetween[N,Label](labelGraph)(source,predecessor) match {
                case None =>
                case Some(predecessorLabel) => {
                  val pathsToPredecessor = predecessorLabel.numShortestPaths.toDouble
                  val pathsToSink = sinkLabel.numShortestPaths.toDouble
                  val newPartial:Double = oldPartial + ((1.0 + nodesToPartialBetweenness.getOrElse(sink,0.0)) * (pathsToPredecessor/pathsToSink))
                  nodesToPartialBetweenness.put(predecessor,newPartial)
                  }
                }
              }
            } //case Some
        }
      }
    }
    (labelGraph,nodesToPartialBetweenness.toMap)
  }

  private def getLabelBetween[N,Label <:Option[BrandesLabel[N]]:ClassTag](labelGraph:MutableGraph[N,MLDiEdge])
                                      (source:labelGraph.NodeT,sink:labelGraph.NodeT):Option[BrandesLabel[N]] = {

    source ~>? sink match {
      case None => None
      case Some(innerEdge) => innerEdge.label.asInstanceOf[Some[PreviousStep[N]]]
    }
  }


  /**
   * This method runs Brande's algorithm for all nodes.
   */
  //todo Can this be a TypeTag? is Manifest needed
  def allPairsShortestPaths[N:Manifest,Label <: Option[BrandesLabel[N]]:ClassTag,Key]
                                                 (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[N,Label])
                                                 (originalGraph:Graph[N,MLDiEdge]):Graph[N,MLDiEdge] = {

    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)
    for(node <- labelGraph.nodes) {
      brandesForSource(labelGraph)(node)(support)
    }
    labelGraph
  }

  /**
   * This method runs Brande's algorithm for all nodes.
   */
  import scala.language.higherKinds

  //todo still need Manifest? Can it be a TypeTag?
  def shortestPathsAndBetweenness[N:Manifest,
                                  Label <: Option[BrandesLabel[N]]:ClassTag,
                                  E[X] <: EdgeLikeIn[X],
                                  Key]
                                  (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[N,Label])
                                  (originalGraph:Graph[N,E]):(Graph[N,MLDiEdge],Map[N,Double]) = {

    val labelGraph:MutableGraph[N,MLDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)
    val partialBetweennesses = for(node <- labelGraph.nodes) yield {
      brandesForSource(labelGraph)(node)(support)._2
    }               //this is a Set[Map[labelGraph.NodeT,Double]]

    def betweennessForNode(node:labelGraph.NodeT):Double = {
      //Can't just use a map and sum here because the results of the map will be a Set, which will only contain one of each value.
      partialBetweennesses.foldLeft(0.0)((r,aMap) => r+aMap.getOrElse(node,0.0))
    }

    val nodesToBetweennesses = labelGraph.nodes.map(node => (node.value,betweennessForNode(node))).toMap

    (labelGraph,nodesToBetweennesses)
  }
}


/**
 * Use this trait for labels where you intend to use Brandes' algorithm. Note that to support Brandes' algorithm the comparator has to be tricked out to avoid double-counting some paths. @See AllShortestPathsPredecessorsSemiring for an example.
 *
 * @tparam N the outer type of nodes in your graph
 */
trait BrandesLabel[N] {
  def predecessors:Set[N]
  def numShortestPaths:Int
  def creator:AnyRef

  def matchingCreator(otherCreator:AnyRef):Boolean = {

    if(otherCreator == creator) true
    else if (otherCreator == BrandesLabel.default) true
    else if (otherCreator == BrandesLabel.originalGraph) true
    else if (creator == BrandesLabel.default) true
    else if (creator == BrandesLabel.originalGraph) true
    else false
  }
}

object BrandesLabel {

  /**
   * Value to use for creator when creating the initial label graph to avoid erasing old results.
   */
  val originalGraph:AnyRef = "original"
  /**
   * Value to use for creator in algorithms where you don't care about dealing with old results.
   */
  val default:AnyRef = "default"
}