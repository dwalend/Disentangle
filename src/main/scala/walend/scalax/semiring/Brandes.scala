package walend.scalax.semiring

import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.mutable.{Graph => MutableGraph}
import walend.scalax.heap.{FibonacciHeap, Heap}
import scala.collection.mutable
import scalax.collection.edge.LBase.LEdgeImplicits
import scala.reflect.ClassTag

/**
 * Brandes' algorithm for betweenness.
 *
 * @author dwalend
 * @since v1
 */
//todo you could find edge betweenness instead with a different algorithm
object Brandes {

  /**
   * Brandes' algorithm.
   */
  //todo set type of label for LDiEdge and remove the instanceOf
  //todo how to express that Label must be Option[BrandesLabel[N]
  def brandesForSource[N:Manifest,Label <: Option[BrandesLabel[N]]:ClassTag,Key](labelGraph:MutableGraph[N,LDiEdge])
                                     (source:labelGraph.NodeT)
                                     (support:GraphMinimizerSupport[Label,Key]):(Graph[N,LDiEdge],Map[labelGraph.NodeT,Double]) = {

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
      //For any node that is reachable from this node not yet visited (because it's key is still in the heap)
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

    println("Find partial betweenness for "+source)

    //todo this seems like it could use a list, an accumulator, and tail recursion
    //todo rename brandeslabel and brandespredecessor to sinklabel and predecessorlabel
    import scala.collection.mutable.{Map => MutableMap}
    val nodesToPartialBetweenness:MutableMap[labelGraph.NodeT,Double] = MutableMap()
    while(!stack.isEmpty) {
      val sink:labelGraph.NodeT = stack.pop()
      println("sink: "+sink)
      //don't bother finding the partial for the sink
      if (sink != source) {
        getLabelBetween[N,Label](labelGraph)(source,sink) match {
          case None =>
          case Some(brandesLabel) => {

            println("brandesLabel: "+brandesLabel)

            for(outerPredecessor <- brandesLabel.predecessors) {

              println("outerPredecessor "+outerPredecessor)

              val predecessor = labelGraph get outerPredecessor
              //add to its partial weight (1 for this sink plus the partial weight accumulated for this sink)*(number of this predecessor's predecessors/number of predecessors)
              val oldPartial:Double = nodesToPartialBetweenness.getOrElse(predecessor,0)
              println("oldPartial is "+oldPartial)
              getLabelBetween[N,Label](labelGraph)(source,predecessor) match {
                case None =>
                case Some(brandesPredecessor) => {
                  //todo pick up here. Look at the numbers going in
                  val pathsToPredecessor = brandesPredecessor.numShortestPaths.toDouble
                  val pathsToSink = brandesLabel.numShortestPaths.toDouble
                  val newPartial:Double = oldPartial + ((1.0 + nodesToPartialBetweenness.getOrElse(sink,0.0)) * (pathsToPredecessor/pathsToSink))
                  println("newPartial is "+newPartial)
                  nodesToPartialBetweenness.put(predecessor,newPartial)
                  }
                }
              }
            } //case Some
        }
      }
    }
    println(source+" nodesToPartialBetweenness is "+nodesToPartialBetweenness)

    (labelGraph,nodesToPartialBetweenness.toMap)
  }

  private def getLabelBetween[N,Label <:Option[BrandesLabel[N]]:ClassTag](labelGraph:MutableGraph[N,LDiEdge])
                                      (source:labelGraph.NodeT,sink:labelGraph.NodeT):Option[BrandesLabel[N]] = {

    object ImplicitLabel extends LEdgeImplicits[Label]
    import ImplicitLabel._

    source ~>? sink match {
      case Some(edge) => edge.label
      case None => None
    }
  }


  /**
   * This method runs Brande's algorithm for all nodes.
   */
  def allPairsShortestPaths[N:Manifest,Label <: Option[BrandesLabel[N]]:ClassTag,Key](originalGraph:Graph[N,LDiEdge])
                                                 (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(support.semiring)
    for(node <- labelGraph.nodes) {
      brandesForSource(labelGraph)(node)(support)
    }
    labelGraph
  }

  /**
   * This method runs Brande's algorithm for all nodes.
   */
  def shortestPathsAndBetweenness[N:Manifest,Label <: Option[BrandesLabel[N]]:ClassTag,Key](originalGraph:Graph[N,LDiEdge])
                                 (support:GraphMinimizerSupport[Label,Key],labelGraphBuilder:LabelGraphBuilder[Label]):(Graph[N,LDiEdge],Map[N,Double]) = {

    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(support.semiring)
    val partialBetweennesses = for(node <- labelGraph.nodes) yield {
      brandesForSource(labelGraph)(node)(support)._2
    }               //this is a Set[Map[labelGraph.NodeT,Double]]
    //todo pick up here and roll up betweennesses

    def betweennessForNode(node:labelGraph.NodeT):Double = {
      //Can't just use a map and sum here because the results of the map will be a Set, which will only contain one of each value.
      partialBetweennesses.foldLeft(0.0)((r,aMap) => r+aMap.getOrElse(node,0.0))
    }

    println("partialBetweennesses are ")
    for(partial <- partialBetweennesses) println(partial)

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
//  def successors:Set[N]   //todo do you really need successors?
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