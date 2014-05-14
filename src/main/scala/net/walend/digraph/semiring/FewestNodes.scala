package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Finds the length of a path that traverses the fewest edges.
 *
 * @author dwalend
 * @since v0.1.0
 */
object FewestNodesSemiring extends Semiring[Int] {

  def I = 0
  def O = Int.MaxValue

  def inDomain(label: Int): Boolean = {
    I <= label && label < O
  }

  def summary(fromThroughToLabel:Int,
              currentLabel:Int):Int = {
    if(fromThroughToLabel < currentLabel) {
      fromThroughToLabel
    }
    else currentLabel
  }

  def extend(fromThroughLabel:Int,throughToLabel:Int):Int = {
    if ((fromThroughLabel == O) || (throughToLabel == O)) O
    else {
      val result = fromThroughLabel + throughToLabel
      if(result < 0) O
      else result
    }
  }

}


/**
 * A heap ordering that puts lower numbers on the top of the heap
 */
object FewestNodesHeapOrdering extends HeapOrdering[Int] {

  def lteq(x: Int, y: Int): Boolean = {
    x >= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Int, y: Int): Option[Int] = {
    Some(y-x)
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Int): Unit = {
    require((FewestNodesSemiring.inDomain(key)||(key == FewestNodesSemiring.O)),"Key must be between zero (included) and Int.MaxValue (excluded), or Int.MaxValue (the annihilator), not "+key)
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Int = -1

  /**
   * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom: Int = Int.MaxValue
}

object FewestNodes extends GraphMinimizerSupport[Int,Int] {

  def semiring = FewestNodesSemiring

  def heapOrdering = FewestNodesHeapOrdering

  def heapKeyForLabel = {label:Label => label}

  def convertEdgeToLabel[Node,Edge,Label](start:Node,end:Node,edge:Edge) = 1

  def convertGraph[Node,Edge](graph:Digraph[Node,Edge],labelForEdge:(Node,Node,Edge)=>Label):Digraph[Node,Label] = {

    val nodes = graph.nodes
    val edges = graph.nodes.map(x => (x,x,FewestNodesSemiring.I)) ++
                  graph.edges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    FastDigraph(edges,nodes,FewestNodesSemiring.O)
  }

  def graphConverter[Node,Edge](graph:Digraph[Node,Edge]) = FewestNodes.convertGraph(graph,FewestNodes.convertEdgeToLabel)

}