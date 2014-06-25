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
class AdjacencyLabelDigraph[Node,Label](outNodes:Vector[Node], //provides the master index values for each node.
                                        outSuccessors:Vector[Vector[(Node,Node,Label)]], // (i) is the successors for node i, (j) is the node,edge pair to reach that second node.
                                       outPredecessors:Vector[Vector[(Node,Node,Label)]],
                                       val noEdgeExistsLabel:Label //value for no edge
                                            ) extends IndexedLabelDigraph[Node,Label] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborVector(vector:Vector[(Node,Node,Label)]):Vector[(InNode,InNode,Label)] = {
    vector.map(x => (nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get,x._3))
  }

  val inSuccessors:Vector[Vector[(InNode,InNode,Label)]] = outSuccessors.map(neighborVector)
  val inPredecessors:Vector[Vector[(InNode,InNode,Label)]] = outPredecessors.map(neighborVector)

  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def successors: Seq[(InNode,InNode,Label)] = {
      inSuccessors(index)
    }

    override def predecessors: Seq[(InNode,InNode,Label)] = {
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
  override def innerNodes: IndexedSeq[InNode] = inNodes

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
    inSuccessors(from.index).filter(x => x._2 == to) match {
      case Vector() => noEdgeExistsLabel
      case Vector(nodeAndEdge) => nodeAndEdge._3
      case x => throw new IllegalStateException(s"Multiple edges from $from to $to: "+x)
    }
  }

  /**
   * O(1)
   *
   * @return
   */
  override def node(i: Int): Node = nodes(i)

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
    inSuccessors(i).filter(x => x._2 == inNodes(j)) match {
      case Vector() => noEdgeExistsLabel
      case Vector(nodeAndEdge) => nodeAndEdge._3
      case x => throw new IllegalStateException(s"Multiple edges from ${inSuccessors(i)} to ${inNodes(j)}: "+x)
    }
  }

  override def toString:String = {
    s"$inNodes $edges"
  }
}

/**
 * O(n ln(n) + e ln(n))
 */
object AdjacencyLabelDigraph{

  def apply[Node,Label](edges:GenTraversable[(Node,Node,Label)] = Seq.empty,
                       extraNodes:GenSeq[Node] = Seq.empty,
                       noEdgeExistsValue:Label = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct.to[Vector]

    val successorMap:GenMap[Node,GenTraversable[(Node,Node,Label)]] = edges.groupBy(_._1)
    val predecessorMap:GenMap[Node,GenTraversable[(Node,Node,Label)]] = edges.groupBy(_._2)

    val successorAdjacencies:Vector[Vector[(Node,Node,Label)]] = nodeValues.map(n => successorMap.getOrElse(n,Vector.empty[(Node,Node,Label)]).to[Vector])
    val predecessorAdjacencies:Vector[Vector[(Node,Node,Label)]] = nodeValues.map(n => predecessorMap.getOrElse(n,Vector.empty[(Node,Node,Label)]).to[Vector])

    new AdjacencyLabelDigraph(nodeValues,successorAdjacencies,predecessorAdjacencies,noEdgeExistsValue)
  }

}