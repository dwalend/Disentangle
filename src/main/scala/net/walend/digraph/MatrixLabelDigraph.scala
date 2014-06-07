package net.walend.digraph

import scala.collection.mutable.ArrayBuffer

/**
 * Provides constant-time access and mutator for arcs. Stores Nodes in a Vector and Arcs in a Vector of ArrayBuffers.
 *
 * The constructor is O(n)
 *
 * @author dwalend
 * @since v0.1.0
 */
//TODO for the Atomic version, make arcMatrix a Vector of AtomicReferences
class MatrixLabelDigraph[Node,Label](outNodes:Vector[Node], //provides the master index values for each node.
                                   arcMatrix:Vector[ArrayBuffer[Label]], // (row,column) is (start,end), indexed by node.
                                   val noArcExistsValue:Label //value for no arc
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
     * O(n^2)
     */
    override def successors: Seq[(InNode,InNode,Label)] = {
      inNodes.zip(arcMatrix(index)).map(x => (this,x._1,x._2)).filter(_._2 != noArcExistsValue)
    }

    /**
     * O(n^2)
     */
    override def predecessors: Seq[(InNode,InNode,Label)] = {
      val arcColumn:Seq[Label] = arcMatrix.map(_(index))
      inNodes.zip(arcColumn).map(x => (x._1,this,x._2)).filter(_._2 != noArcExistsValue)
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
   * @return All of the arcs in the graph
   */
  override def arcs: Seq[(Node, Node, Label)] = {

    def arcsInRow(row:(ArrayBuffer[Label],Int)):Seq[(Node,Node,Label)] = {
      val rowIndex = row._2
      val cellsWithIndex = row._1.zipWithIndex
      val cellsWithArcs = cellsWithIndex.filter(x => (x._1 != noArcExistsValue))
      cellsWithArcs.map(x => (outNodes(rowIndex),outNodes(x._2),x._1))
    }

    arcMatrix.zipWithIndex.map(row => arcsInRow(row)).flatten
  }

  /**
   * O(1)
   *
   * @return the Arc between start and end or noArcExistsValue
   */
  override def label(from: InNode, to: InNode):Label = arcMatrix(from.index)(to.index)
 

  /**
   * O(1)
   */
  override def upsertArc(from: InNode, to: InNode, arc: Label): Unit = arcMatrix(from.index)(to.index) = arc


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
  override def label(i: Int, j: Int): Label = arcMatrix(i)(j)

}

object MatrixLabelDigraph{

  /**
   * O(n ln(n) + en)
   */
  def apply[Node,Arc](arcSeq:Seq[(Node,Node,Arc)] = Seq.empty,
                       extraNodes:Seq[Node] = Seq.empty,
                       noArcExistsValue:Arc = null) = {

    val nodeValues:Vector[Node] = (extraNodes ++ arcSeq.map(_._1) ++ arcSeq.map(_._2)).distinct.to[Vector]

    val size = nodeValues.size

    val matrix:Vector[ArrayBuffer[Arc]] = nodeValues.map(x => ArrayBuffer.fill(size)(noArcExistsValue)).to[Vector]

    for (arcTriple <- arcSeq) {
      val row = nodeValues.indexOf(arcTriple._1)
      val column = nodeValues.indexOf(arcTriple._2)

      assert(matrix(row)(column)==noArcExistsValue,s"arcSeq includes two arcs between ${arcTriple._1} and ${arcTriple._2}, ${matrix(row)(column)} and ${arcTriple._3}")

      matrix(row)(column) = arcTriple._3
    }

    new MatrixLabelDigraph(nodeValues,matrix,noArcExistsValue)
  }

}