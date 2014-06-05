package net.walend.digraph

/**
 * Provides constant-time access for successor and predecessor arcs of a node.
 *
 * The constructor is O(n + a ln(n))
 *
 * @author dwalend
 * @since v0.1.0
 */
class AdjacencyDigraph[Node,Arc](outNodes:Vector[Node], //provides the master index values for each node.
                                   outSuccessors:Vector[Vector[(Node,Node,Arc)]], // (i) is the successors for node i, (j) is the node,arc pair to reach that second node.
                                   outPredecessors:Vector[Vector[(Node,Node,Arc)]],
                                   val noArcExistsValue:Arc //value for no arc
                                            ) extends IndexedDigraph[Node,Arc] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborVector(vector:Vector[(Node,Node,Arc)]):Vector[(InNode,InNode,Arc)] = {
    vector.map(x => (nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get,x._3))
  }

  val inSuccessors:Vector[Vector[(InNode,InNode,Arc)]] = outSuccessors.map(neighborVector)
  val inPredecessors:Vector[Vector[(InNode,InNode,Arc)]] = outPredecessors.map(neighborVector)

  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def successors: Seq[(InNode,InNode,Arc)] = {
      inSuccessors(index)
    }

    override def predecessors: Seq[(InNode,InNode,Arc)] = {
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
   * @return All of the arcs in the graph
   */
  override def arcs: Seq[(Node, Node, Arc)] = outSuccessors.flatten

  /**
   * O(n)
   *
   * @return the Arc between start and end or noArcExistsValue
   */
  override def arc(from: InNode, to: InNode):Arc = {
    inSuccessors(from.index).filter(x => x._2 == to) match {
      case Vector() => noArcExistsValue
      case Vector(nodeAndArc) => nodeAndArc._3
      case x => throw new IllegalStateException(s"Multiple arcs from $from to $to: "+x)
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
  override def arc(i: Int, j: Int): Arc = {
    inSuccessors(i).filter(x => x._2 == inNodes(j)) match {
      case Vector() => noArcExistsValue
      case Vector(nodeAndArc) => nodeAndArc._3
      case x => throw new IllegalStateException(s"Multiple arcs from ${inSuccessors(i)} to ${inNodes(j)}: "+x)
    }
  }

  override def toString():String = {
    s"$inNodes $arcs"
  }

}

/**
 * O(n ln(n) + e ln(n))
 */
object AdjacencyDigraph{

  def apply[Node,Arc](arcSeq:Seq[(Node,Node,Arc)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noArcExistsValue:Arc = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ arcSeq.map(_._1) ++ arcSeq.map(_._2)).distinct.to[Vector]

    val successorMap:Map[Node,Seq[(Node,Node,Arc)]] = arcSeq.groupBy(_._1)
    val predecessorMap:Map[Node,Seq[(Node,Node,Arc)]] = arcSeq.groupBy(_._2)

    val successorAdjacencies:Vector[Vector[(Node,Node,Arc)]] = nodeValues.map(n => successorMap.getOrElse(n,Vector.empty[(Node,Node,Arc)]).to[Vector])
    val predecessorAdjacencies:Vector[Vector[(Node,Node,Arc)]] = nodeValues.map(n => predecessorMap.getOrElse(n,Vector.empty[(Node,Node,Arc)]).to[Vector])

    new AdjacencyDigraph(nodeValues,successorAdjacencies,predecessorAdjacencies,noArcExistsValue)
  }

}