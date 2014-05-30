package net.walend.digraph

/**
 * Provides constant-time access for successor and predecessor edges for a node.
 *
 * The constructor is O(n + e ln(n))
 *
 * @author dwalend
 * @since v0.1.0
 */
class AdjacencyDigraph[Node,Edge](outNodes:Vector[Node], //provides the master index values for each node.
                                   outSuccessors:Vector[Vector[(Node,Node,Edge)]], // (i) is the successors for node i, (j) is the node,edge pair to reach that second node.
                                   outPredecessors:Vector[Vector[(Node,Node,Edge)]],
                                   val noEdgeExistsValue:Edge //value for no edge
                                            ) extends IndexedDigraph[Node,Edge] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborVector(vector:Vector[(Node,Node,Edge)]):Vector[(InNode,InNode,Edge)] = {
    vector.map(x => (nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get,x._3))
  }

  val inSuccessors:Vector[Vector[(InNode,InNode,Edge)]] = outSuccessors.map(neighborVector)
  val inPredecessors:Vector[Vector[(InNode,InNode,Edge)]] = outPredecessors.map(neighborVector)

  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def successors: Seq[(InNode,InNode,Edge)] = {
      inSuccessors(index)
    }

    override def predecessors: Seq[(InNode,InNode,Edge)] = {
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

  /**
   * O(n)
   *
   * @return All of the edges in the graph
   */
  override def edges: Seq[(Node, Node, Edge)] = outSuccessors.flatten

  /**
   * O(n)
   *
   * @return the Edge between start and end or noEdgeExistsValue
   */
  override def edge(from: InNode, to: InNode):Edge = {
    inSuccessors(from.index).filter(x => x._2 == to) match {
      case Vector() => noEdgeExistsValue
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
  override def edge(i: Int, j: Int): Edge = {
    inSuccessors(i).filter(x => x._2 == inNodes(j)) match {
      case Vector() => noEdgeExistsValue
      case Vector(nodeAndEdge) => nodeAndEdge._3
      case x => throw new IllegalStateException(s"Multiple edges from ${inSuccessors(i)} to ${inNodes(j)}: "+x)
    }
  }

}

/**
 * O(n ln(n) + e ln(n))
 */
object AdjacencyDigraph{

  def apply[Node,Edge](edgeSeq:Seq[(Node,Node,Edge)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Edge = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edgeSeq.map(_._1) ++ edgeSeq.map(_._2)).distinct.to[Vector]

    val successorMap:Map[Node,Seq[(Node,Node,Edge)]] = edgeSeq.groupBy(_._1)
    val predecessorMap:Map[Node,Seq[(Node,Node,Edge)]] = edgeSeq.groupBy(_._2)

    val successorAdjacencies:Vector[Vector[(Node,Node,Edge)]] = nodeValues.map(n => successorMap.getOrElse(n,Vector.empty[(Node,Node,Edge)]).to[Vector])
    val predecessorAdjacencies:Vector[Vector[(Node,Node,Edge)]] = nodeValues.map(n => predecessorMap.getOrElse(n,Vector.empty[(Node,Node,Edge)]).to[Vector])

    new AdjacencyDigraph(nodeValues,successorAdjacencies,predecessorAdjacencies,noEdgeExistsValue)
  }

}