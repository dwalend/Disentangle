package net.walend.digraph

import scala.collection.mutable.ArrayBuffer

/**
 * Provides constant-time access and mutators for edges. Stores Nodes in a Vector and Edges in a Vector of ArrayBuffers.
 *
 * The constructor is O(n)
 *
 * @author dwalend
 * @since v0.1.0
 */
//TODO for the Atomic version, make edgeMatrix a Vector of AtomicReferences
class MatrixDigraph[Node,Edge](outNodes:Vector[Node], //provides the master index values for each node.
                               edgeMatrix:Vector[ArrayBuffer[Edge]], // (row,column) is (start,end), indexed by node.
                               val noEdgeExistsValue:Edge //value for no edge
                                ) extends IndexedDigraph[Node,Edge] with MutableEdgeDigraph[Node,Edge] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap
  /**
   * O(1)
   */
  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    /**
     * O(n^2)
     */
    override def successors: Seq[(InNode,InNode,Edge)] = {
      inNodes.zip(edgeMatrix(index)).map(x => (this,x._1,x._2)).filter(_._2 != noEdgeExistsValue)
    }

    /**
     * O(n^2)
     */
    override def predecessors: Seq[(InNode,InNode,Edge)] = {
      val edgeColumn:Seq[Edge] = edgeMatrix.map(_(index))
      inNodes.zip(edgeColumn).map(x => (x._1,this,x._2)).filter(_._2 != noEdgeExistsValue)
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
   */
  override def innerNode(value: Node): Option[InNode] = {
    nodeToInNode.get(value)
  }

  /**
   * O(n)
   * @return InnerNode representation of all of the nodes in the graph.
   */
  override def innerNodes: IndexedSeq[InNode] = {
    outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  }

  /**
   * O(n^2)
   *
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

  /**
   * O(1)
   *
   * @return the Edge between start and end or noEdgeExistsValue
   */
  override def edge(from: InNode, to: InNode):Edge = {

    edgeMatrix(from.index)(to.index)
  }

  /**
   * O(1)
   */
  override def updateEdge(from: InNode, to: InNode, edge: Edge): Unit = {
    edgeMatrix(from.index)(to.index) = edge
  }

  /**
   * O(1)
   */
  override def node(i: Int): Node = nodes(i)

  /**
   * O(1)
   */
  override def innerNodeForIndex(i: Int): InNode = innerNodes(i)

  /**
   * O(1)
   */
  override def edge(i: Int, j: Int): Edge = edgeMatrix(i)(j)

}

object MatrixDigraph{

  /**
   * O(n ln(n) + en)
   */
  def apply[Node,Edge](edgeSeq:Seq[(Node,Node,Edge)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Edge = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edgeSeq.map(_._1) ++ edgeSeq.map(_._2)).distinct.to[Vector]

    val size = nodeValues.size

    val matrix:Vector[ArrayBuffer[Edge]] = nodeValues.map(x => ArrayBuffer.fill(size)(noEdgeExistsValue)).to[Vector]

    for (edgeTriple <- edgeSeq) {
      val row = nodeValues.indexOf(edgeTriple._1)
      val column = nodeValues.indexOf(edgeTriple._2)

      assert(matrix(row)(column)==noEdgeExistsValue,s"edgeSeq includes two edges between ${edgeTriple._1} and ${edgeTriple._2}, ${matrix(row)(column)} and ${edgeTriple._3}")

      matrix(row)(column) = edgeTriple._3
    }

    new MatrixDigraph(nodeValues,matrix,noEdgeExistsValue)
  }

}