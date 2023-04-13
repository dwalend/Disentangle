package net.walend.disentangle.graph.mutable

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Seq, Iterable}

import net.walend.disentangle.graph.{IndexedLabelDigraph, IndexedSet}

/**
 * Provides constant-time access and mutator for edges. Stores Nodes in a Vector and Labels in a Vector of ArrayBuffers.
 *
 * The constructor is O(n)
 *
 * @author dwalend
 * @since v0.1.0
 */
case class MatrixLabelDigraph[Node,Label](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                           edgeMatrix:Vector[ArrayBuffer[Label]], // (row,column) is (start,end), indexed by node.
                                           noEdgeExistsLabel:Label //value for no edge
                                          ) extends IndexedLabelDigraph[Node,Label] with MutableLabelDigraph[Node,Label] {

  override type InnerNodeType = InnerNode
  case class InnerNode(override val value:Node, override val index:Int) extends this.DigraphInnerNodeTrait with this.InnerIndexedNodeTrait {

    /**
      * O(n&#94;2)
      */
    override def successors: IndexedSet[InnerEdgeType] = {
      inNodes.zip(edgeMatrix(index)).map(x => InnerEdge(this,x._1,x._2)).filter(_.to != noEdgeExistsLabel)
    }

    /**
      * O(n&#94;2)
      */
    override def predecessors: IndexedSet[InnerEdgeType]  = {
      val edgeColumn:Seq[Label] = edgeMatrix.map(_(index))
      inNodes.zip(edgeColumn).map(x => InnerEdge(x._1,this,x._2)).filter(_.to != noEdgeExistsLabel)
    }

    override def hashCode(): Int = index

    override def equals(obj: Any): Boolean = {
      obj match {
        case inNode:InnerNode => inNode.index == index
        case _ => false
      }
    }
  }

  override type InnerEdgeType = InnerEdge
  case class InnerEdge(from:InnerNodeType,to:InnerNodeType,label:Label) extends LabelDigraphEdgeTrait {
    override def value: OuterEdgeType = (from.value,to.value,label)
  }

  val inNodes:IndexedSet[InnerNode] = outNodes.zipWithIndex.map(x => InnerNode(x._1,x._2))
  val nodeToInNode:Map[Node,InnerNode] = inNodes.map(x => x.value -> x).toMap

  /**
    * O(1)
    */
  override def nodes = outNodes

  /**
   * O(1)
   */
  override def nodeCount: Int = outNodes.size

  /**
   * O(ln(n))
   */
  override def innerNode(value: Node): Option[InnerNode] = {
    nodeToInNode.get(value)
  }

  /**
   * O(n)
    *
    * @return InnerNode representation of all of the nodes in the graph.
   */
  override def innerNodes: IndexedSet[InnerNode] = {
    outNodes.zipWithIndex.map(x => InnerNode(x._1,x._2))
  }

  /**
   * @return A Traversable of the edges as represented in the graph
   */
  override def innerEdges: Vector[InnerEdgeType] = {

    def edgesInRow(row:(ArrayBuffer[Label],Int)):Seq[InnerEdgeType] = {
      val rowIndex = row._2
      val cellsWithIndex = row._1.zipWithIndex
      val cellsWithEdges = cellsWithIndex.filter(x => (x._1 != noEdgeExistsLabel))
      Seq.from(cellsWithEdges.map(x => InnerEdge(inNodes.apply(rowIndex),inNodes.apply(x._2),x._1)))
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
      Seq.from(cellsWithEdges.map(x => (outNodes.apply(rowIndex),outNodes.apply(x._2),x._1)))
    }

    edgeMatrix.zipWithIndex.map(row => edgesInRow(row)).flatten
  }

  /**
   * O(1)
   *
   * @return the edge between start and end or noEdgeExistsValue
   */
  override def label(from: InnerNode, to: InnerNode):Label = edgeMatrix(from.index)(to.index)


  /**
   * O(1)
   */
  override def upsertEdge(from: InnerNode, to: InnerNode, label: Label): Unit = edgeMatrix(from.index)(to.index) = label


  /**
   * O(1)
   */
  override def node(i: Int): Node = outNodes.apply(i)

  /**
   * O(1)
   */
  override def innerNodeForIndex(i: Int): InnerNode = innerNodes.apply(i)

  /**
   * O(1)
   */
  override def label(i: Int, j: Int): Label = edgeMatrix(i)(j)

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes,noEdgeExistsValue = $noEdgeExistsLabel)"
  }

  override def edge(from: InnerNode, to: InnerNode): Option[InnerEdge] = {
    val l = label(from.index,to.index)
    if(noEdgeExistsLabel == l) None
    else Some(InnerEdge(from,to,l))
  }
}

object MatrixLabelDigraph{

  /**
   * O(n ln(n) + en)
   */
  def apply[Node,Label](edges:Iterable[(Node,Node,Label)] = Seq.empty,
                       nodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Label = null) = {

    val nodeValues:IndexedSet[Node] = IndexedSet.from((nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct)

    val size = nodeValues.size

    val matrix:Vector[ArrayBuffer[Label]] = Vector.from(nodeValues.asSeq.map(x => ArrayBuffer.fill(size)(noEdgeExistsValue)))

    for (edgeTriple <- edges) {
      val row = nodeValues.indexOf(edgeTriple._1)
      val column = nodeValues.indexOf(edgeTriple._2)

      assert(matrix(row)(column)==noEdgeExistsValue,s"edges includes two edges between ${edgeTriple._1} and ${edgeTriple._2}, ${matrix(row)(column)} and ${edgeTriple._3}")

      matrix(row)(column) = edgeTriple._3
    }

    new MatrixLabelDigraph(nodeValues,matrix,noEdgeExistsValue)
  }

}