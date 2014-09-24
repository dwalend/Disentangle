package net.walend.graph

import scala.collection.{GenMap, GenSeq, GenTraversable}

/**
 * Provides constant-time access for successor and predecessor edges of a node.
 *
 * The constructor is O(n + a ln(n))
 *
 * @author dwalend
 * @since v0.1.0
 */
class AdjacencyLabelDigraph[Node,Label](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                        outSuccessors:Vector[IndexedSet[(Node,Node,Label)]], // (i) is the successors for node i, (j) is the node,edge pair to reach that second node.
                                       outPredecessors:Vector[IndexedSet[(Node,Node,Label)]],
                                       val noEdgeExistsLabel:Label //value for no edge
                                            ) extends IndexedLabelDigraph[Node,Label] {

  val inNodes:IndexedSet[InNode] =outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborSet(indexedSet:IndexedSet[(Node,Node,Label)]):IndexedSet[(InNode,InNode,Label)] = {
    indexedSet.map(x => (nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get,x._3))
  }

  val inSuccessors:Vector[IndexedSet[(InNode,InNode,Label)]] = outSuccessors.map(neighborSet)
  val inPredecessors:Vector[IndexedSet[(InNode,InNode,Label)]] = outPredecessors.map(neighborSet)

  def nodes = outNodes

  //todo remove after 0.1.1
  @deprecated("Will be removed after 0.1.1","0.1.1")
  def nodesSeq: IndexedSeq[Node] = outNodes.asSeq

  override def nodeCount: Int = outNodes.size

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    //todo remove
    override def successorsAsSeq: Seq[(InNode,InNode,Label)] = {
      inSuccessors(index).asSeq
    }

    //todo remove
    override def predecessorsAsSeq: Seq[(InNode,InNode,Label)] = {
      inPredecessors(index).asSeq
    }

    override def successors: IndexedSet[(InNode,InNode,Label)] = {
      inSuccessors(index)
    }

    override def predecessors: IndexedSet[(InNode,InNode,Label)] = {
      inPredecessors(index)
    }

    override def hashCode(): Int = index

    override def equals(obj: Any): Boolean = {
      obj match {
        case inNode:InNode => inNode.index == index
        case _ => false
      }
    }

  }

  /**
   * O(ln(n))
   *
   * @return Some inner node if it exists in the digraph or None
   */
  override def innerNode(value: Node): Option[InNode] = {
    nodeToInNode.get(value)
  }

  /**
   * O(1)
   *
   * @return InnerNode representation of all of the nodes in the graph.
   */
  override def innerNodes: IndexedSet[InNode] = inNodes

  override type InnerEdgeType = (InNode,InNode,Label)

  /**
   * @return A Traversable of the edges as represented in the graph
   */
  override def innerEdges:Vector[InnerEdgeType] = inSuccessors.flatten

  /**
   * O(n&#94;2)
   *
   * @return All of the edges in the graph
   */
  override def edges: Seq[OuterEdgeType] = outSuccessors.flatten

  /**
   * O(n)
   *
   * @return the edge between start and end or noEdgeExistsValue
   */
  override def label(from: InNode, to: InNode):Label = {
    val indexedSet = inSuccessors(from.index).filter(x => x._2 == to)
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next()._3
      case _ => throw new IllegalStateException(s"Multiple edges from $from to $to: "+indexedSet)
    }
  }

  /**
   * O(1)
   *
   * @return
   */
  override def node(i: Int): Node = outNodes(i)

  /**
   * O(1)
   *
   * @return
   */
  override def innerNodeForIndex(i: Int): InNode = innerNodes(i)

  /**
   * O(n)
   *
   * @return
   */
  override def label(i: Int, j: Int): Label = {
    val indexedSet = inSuccessors(i).filter(x => x._2 == inNodes(j))
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next()._3
      case _ => throw new IllegalStateException(s"Multiple edges from ${node(i)} to ${node(j)}: "+indexedSet)
    }
  }

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes,noEdgeExistsValue = $noEdgeExistsLabel)"
  }
}

/**
 * O(n ln(n) + e ln(n))
 */
object AdjacencyLabelDigraph{

  def apply[Node,Label](edges:GenTraversable[(Node,Node,Label)] = Seq.empty,
                       nodes:GenSeq[Node] = Seq.empty,
                       noEdgeExistsValue:Label = null) = {

    val nodeValues:Vector[Node] = (nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct.to[Vector]

    val successorMap:GenMap[Node,GenTraversable[(Node,Node,Label)]] = edges.groupBy(_._1)
    val predecessorMap:GenMap[Node,GenTraversable[(Node,Node,Label)]] = edges.groupBy(_._2)

    val successorAdjacencies:Vector[IndexedSet[(Node,Node,Label)]] = nodeValues.map(n => successorMap.getOrElse(n,Vector.empty[(Node,Node,Label)]).to[IndexedSet])
    val predecessorAdjacencies:Vector[IndexedSet[(Node,Node,Label)]] = nodeValues.map(n => predecessorMap.getOrElse(n,Vector.empty[(Node,Node,Label)]).to[IndexedSet])

    new AdjacencyLabelDigraph(nodeValues.to[IndexedSet],successorAdjacencies,predecessorAdjacencies,noEdgeExistsValue)
  }

}