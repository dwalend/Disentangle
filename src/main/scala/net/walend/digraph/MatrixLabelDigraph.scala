package net.walend.digraph

import scala.collection.mutable.ArrayBuffer
import scala.collection.{GenSeq, GenTraversable}

/**
 * Provides constant-time access and mutator for edges. Stores Nodes in a Vector and Labels in a Vector of ArrayBuffers.
 *
 * The constructor is O(n)
 *
 * @author dwalend
 * @since v0.1.0
 */
//TODO for the Atomic version, make edgeMatrix a Vector of AtomicReferences
class MatrixLabelDigraph[Node,Label](outNodes:Vector[Node], //provides the master index values for each node.
                                   edgeMatrix:Vector[ArrayBuffer[Label]], // (row,column) is (start,end), indexed by node.
                                   val noEdgeExistsLabel:Label //value for no edge
                                    ) extends IndexedLabelDigraph[Node,Label] with MutableLabelDigraph[Node,Label] {

  val inNodes:Vector[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap
  /**
   * O(1)
   */
  override def nodes: IndexedSeq[Node] = outNodes

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    /**
     * O(n&#94;2)
     */
    override def successors: Seq[(InNode,InNode,Label)] = {
      inNodes.zip(edgeMatrix(index)).map(x => (this,x._1,x._2)).filter(_._2 != noEdgeExistsLabel)
    }

    /**
     * O(n&#94;2)
     */
    override def predecessors: Seq[(InNode,InNode,Label)] = {
      val edgeColumn:Seq[Label] = edgeMatrix.map(_(index))
      inNodes.zip(edgeColumn).map(x => (x._1,this,x._2)).filter(_._2 != noEdgeExistsLabel)
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

  override type InnerEdgeType = (InNode,InNode,Label)

  /**
   * @return A Traversable of the edges as represented in the graph
   */
  override def innerEdges: Vector[InnerEdgeType] = {

    def edgesInRow(row:(ArrayBuffer[Label],Int)):Seq[(InNode,InNode,Label)] = {
      val rowIndex = row._2
      val cellsWithIndex = row._1.zipWithIndex
      val cellsWithEdges = cellsWithIndex.filter(x => (x._1 != noEdgeExistsLabel))
      cellsWithEdges.map(x => (inNodes(rowIndex),inNodes(x._2),x._1))
    }

    edgeMatrix.zipWithIndex.map(row => edgesInRow(row)).flatten
  }

  /**
   * O(n&#94;2)
   *
   * @return All of the edges in the graph
   */
  override def edges: Vector[(Node, Node, Label)] = {

    def edgesInRow(row:(ArrayBuffer[Label],Int)):Seq[(Node,Node,Label)] = {
      val rowIndex = row._2
      val cellsWithIndex = row._1.zipWithIndex
      val cellsWithEdges = cellsWithIndex.filter(x => (x._1 != noEdgeExistsLabel))
      cellsWithEdges.map(x => (outNodes(rowIndex),outNodes(x._2),x._1))
    }

    edgeMatrix.zipWithIndex.map(row => edgesInRow(row)).flatten
  }

  /**
   * O(1)
   *
   * @return the edge between start and end or noEdgeExistsValue
   */
  override def label(from: InNode, to: InNode):Label = edgeMatrix(from.index)(to.index)
 

  /**
   * O(1)
   */
  override def upsertEdge(from: InNode, to: InNode, label: Label): Unit = edgeMatrix(from.index)(to.index) = label


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
  override def label(i: Int, j: Int): Label = edgeMatrix(i)(j)

}

object MatrixLabelDigraph{

  /**
   * O(n ln(n) + en)
   */
  def apply[Node,Label](edges:GenTraversable[(Node,Node,Label)] = Seq.empty,
                       extraNodes:GenSeq[Node] = Seq.empty,
                       noEdgeExistsValue:Label = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct.to[Vector]

    val size = nodeValues.size

    val matrix:Vector[ArrayBuffer[Label]] = nodeValues.map(x => ArrayBuffer.fill(size)(noEdgeExistsValue)).to[Vector]

    for (edgeTriple <- edges) {
      val row = nodeValues.indexOf(edgeTriple._1)
      val column = nodeValues.indexOf(edgeTriple._2)

      assert(matrix(row)(column)==noEdgeExistsValue,s"edges includes two edges between ${edgeTriple._1} and ${edgeTriple._2}, ${matrix(row)(column)} and ${edgeTriple._3}")

      matrix(row)(column) = edgeTriple._3
    }

    new MatrixLabelDigraph(nodeValues,matrix,noEdgeExistsValue)
  }

}