package net.walend.digraph

import scala.collection.mutable.ArrayBuffer

/**
 * Provides constant-time access for successor and predecessor edges for a node.
 *
 * @author dwalend
 * @since v0.1.0
 */
/**
  */
class AdjacencyDigraph[Node,Edge](outNodes:Vector[Node], //provides the master index values for each node.
                                   outSuccessors:Vector[Vector[(Node,Edge)]], // (i) is the successors for node i, (j) is the node,edge pair to reach that second node.
                                   outPredecessors:Vector[Vector[(Node,Edge)]],
                                   val noEdgeExistsValue:Edge //value for no edge
                                            ) extends IndexedDigraph[Node,Edge] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborVector(vector:Vector[(Node,Edge)]):Vector[(InNode,Edge)] = {
    vector.map(x => (nodeToInNode.get(x._1).get,x._2))
  }

  val inSuccessors:Vector[Vector[(InNode,Edge)]] = outSuccessors.map(neighborVector)
  val inPredecessors:Vector[Vector[(InNode,Edge)]] = outPredecessors.map(neighborVector)

  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def successors: Seq[(InNode,Edge)] = {
      inSuccessors(index)
    }

    override def predecessors: Seq[(InNode,Edge)] = {
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

  override def innerNode(value: Node): Option[InNode] = {
    nodeToInNode.get(value)
  }

  override def innerNodes: IndexedSeq[InNode] = inNodes

  /**
   * @return All of the edges in the graph
   */
  override def edges: Seq[(Node, Node, Edge)] = {

    def edgesForSuccessors(nodeAndSuccessors:(Node,Vector[(Node,Edge)])):Vector[(Node,Node,Edge)] = {
      nodeAndSuccessors._2.map(x => (nodeAndSuccessors._1,x._1,x._2))
    }

    outNodes.zip(outSuccessors).map(edgesForSuccessors).flatten
  }

  //todo apply?
  override def edge(from: InNode, to: InNode):Edge = {
    inSuccessors(from.index).filter(x => x._1 == to) match {
      case Vector() => noEdgeExistsValue
      case Vector(nodeAndEdge) => nodeAndEdge._2
      case x => throw new IllegalStateException(s"Multiple edges from $from to $to: "+x)
    }
  }

  override def node(i: Int): Node = nodes(i)

  override def innerNodeForIndex(i: Int): InNode = innerNodes(i)

  override def edge(i: Int, j: Int): Edge = {
    inSuccessors(i).filter(x => x._1 == inNodes(j)) match {
      case Vector() => noEdgeExistsValue
      case Vector(nodeAndEdge) => nodeAndEdge._2
      case x => throw new IllegalStateException(s"Multiple edges from ${inSuccessors(i)} to ${inNodes(j)}: "+x)
    }
  }

}

object AdjacencyDigraph{

  def apply[Node,Edge](edgeSeq:Seq[(Node,Node,Edge)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Edge = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edgeSeq.map(_._1) ++ edgeSeq.map(_._2)).distinct.to[Vector]

    val size = nodeValues.size

    val successorAdjacencyBuffers:Vector[ArrayBuffer[(Node,Edge)]] = nodeValues.map(x => ArrayBuffer[(Node,Edge)]())
    val predecessorAdjacencyBuffers:Vector[ArrayBuffer[(Node,Edge)]] = nodeValues.map(x => ArrayBuffer[(Node,Edge)]())

    for (edgeTriple <- edgeSeq) {
      val fromIndex = nodeValues.indexOf(edgeTriple._1)
      val toIndex = nodeValues.indexOf(edgeTriple._2)

      successorAdjacencyBuffers(fromIndex) += ((edgeTriple._2,edgeTriple._3))
      predecessorAdjacencyBuffers(toIndex) += ((edgeTriple._1,edgeTriple._3))

      //todo check for multiple edges between the same start and end nodes
    }
    val successorAdjacencies:Vector[Vector[(Node,Edge)]] = successorAdjacencyBuffers.map(_.to[Vector])
    val predecessorAdjacencies:Vector[Vector[(Node,Edge)]] = predecessorAdjacencyBuffers.map(_.to[Vector])

    new AdjacencyDigraph(nodeValues,successorAdjacencies,predecessorAdjacencies,noEdgeExistsValue)
  }

}