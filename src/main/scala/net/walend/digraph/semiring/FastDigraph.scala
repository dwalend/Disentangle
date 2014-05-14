package net.walend.digraph.semiring

import scala.collection.mutable.ArrayBuffer

/**
 * Provides constant-time access and mutators for edges. Stores Nodes in a Vector and Edges in an ArrayBuffer of ArrayBuffers.
 *
 * @author dwalend
 * @since v0.1.0
 */
/**
 */
//TODO store edge indices in an ArrayBuffer per node to allow for constant-tme successors and predecessors

class FastDigraph[Node,Edge](outNodes:Vector[Node], //provides the master index values for each node.
                             edgeMatrix:ArrayBuffer[ArrayBuffer[Edge]], // (row,column) is (from,to), indexed by node.
                             val noEdgeExistsValue:Edge //value for no edge
                              ) extends Digraph[Node,Edge] {

  override def nodes: Seq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,index:Int) extends this.InnerNodeTrait {
    //todo validity check if you ever allow removing nodes
  }

  //todo maybe an InNode cache?
  override def innerNode(value: Node): Option[InNode] = {
    val index:Int = outNodes.indexOf(value)
    if(index == -1) None
    else Some(InNode(value,index))
  }

  override def innerNodes: Seq[InNode] = {
    outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  }

  /**
   * @return All of the edges in the graph
   */
  override def edges: Seq[(Node, Node, Edge)] = {

    def edgesInRow(row:(ArrayBuffer[Edge],Int)):Seq[(Node,Node,Edge)] = {
      val rowIndex = row._2
      val cellsWithIndex = row._1.zipWithIndex
      val cellsWithEdges = cellsWithIndex.filter(x => (x._1 != noEdgeExistsValue))
      cellsWithEdges.map(x => (outNodes(rowIndex),outNodes(x._2),x._1))
    }

    edgeMatrix.zipWithIndex.map(row => edgesInRow(row)).flatten
  }

  //todo apply?
  override def edge(from: InNode, to: InNode):Edge = {

    edgeMatrix(from.index)(to.index)
  }

  override def updateEdge(from: InNode, to: InNode, edge: Edge): Unit = {
    edgeMatrix(from.index)(to.index) = edge
  }

}

object FastDigraph{

  def apply[Node,Edge](edgeSeq:Seq[(Node,Node,Edge)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Edge = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edgeSeq.map(_._1) ++ edgeSeq.map(_._2)).distinct.to[Vector]

    val size = nodeValues.size

    val matrix:ArrayBuffer[ArrayBuffer[Edge]] = nodeValues.map(x => ArrayBuffer.fill(size)(noEdgeExistsValue)).to[ArrayBuffer]

    for (edgeTriple <- edgeSeq) {
      val row = nodeValues.indexOf(edgeTriple._1)
      val column = nodeValues.indexOf(edgeTriple._2)

      assert(matrix(row)(column)==noEdgeExistsValue,s"edgeSeq includes two edges between ${edgeTriple._1} and ${edgeTriple._2}, ${matrix(row)(column)} and ${edgeTriple._3}")

      matrix(row)(column) = edgeTriple._3
    }

    new FastDigraph(nodeValues,matrix,noEdgeExistsValue)
  }

}