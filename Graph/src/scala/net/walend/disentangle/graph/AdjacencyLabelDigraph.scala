package net.walend.disentangle.graph

import scala.collection.immutable.{Map, Seq, Iterable}

/**
 * Provides constant-time access for successor and predecessor edges of a node.
 *
 * The constructor is O(n + a ln(n))
 *
 * @author dwalend
 * @since v0.1.0
 */
class AdjacencyLabelDigraph[Node,Label](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                        outSuccessors:Vector[IndexedSet[(Node,Node,Label)]], // (i) is the successors for node i, (j) is the node,node,label tuple to reach that second node.
                                       outPredecessors:Vector[IndexedSet[(Node,Node,Label)]],
                                       val noEdgeExistsLabel:Label //value for no edge
                                            ) extends IndexedLabelDigraph[Node,Label] {
  override type InnerNodeType = InnerNode
  case class InnerNode(override val value:Node, override val index:Int) extends this.DigraphInnerNodeTrait with this.InnerIndexedNodeTrait {

    override def successors: IndexedSet[InnerEdgeType] = {
      inSuccessors(index)
    }

    override def predecessors: IndexedSet[InnerEdgeType] = {
      inPredecessors(index)
    }

    override def hashCode(): Int = index

    override def equals(thing: Any): Boolean = {
      thing match {
        case inNode:InnerNode => inNode.index == index
        case _ => false
      }
    }
  }

  override type InnerEdgeType = InnerEdge
  case class InnerEdge(from:InnerNodeType,to:InnerNodeType,label:Label) extends LabelDigraphEdgeTrait {
    override def value: OuterEdgeType = (from.value,to.value,label)
  }

  val inNodes:IndexedSet[InnerNode] =outNodes.zipWithIndex.map(x => InnerNode(x._1,x._2))
  val nodeToInNode:Map[Node,InnerNode] = inNodes.map(x => x.value -> x).toMap

  def neighborSet(indexedSet:IndexedSet[(Node,Node,Label)]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(x => InnerEdge(nodeToInNode(x._1),nodeToInNode(x._2),x._3))
  }

  val inSuccessors:Vector[IndexedSet[InnerEdgeType]] = outSuccessors.map(neighborSet)
  val inPredecessors:Vector[IndexedSet[InnerEdgeType]] = outPredecessors.map(neighborSet)

  def nodes: IndexedSet[Node] = outNodes

  override def nodeCount: Int = outNodes.size

  /**
   * O(ln(n))
   *
   * @return Some inner node if it exists in the digraph or None
   */
  override def innerNode(value: Node): Option[InnerNode] = {
    nodeToInNode.get(value)
  }

  /**
   * O(1)
   *
   * @return InnerNode representation of all of the nodes in the graph.
   */
  override def innerNodes: IndexedSet[InnerNode] = inNodes

  /**
   * @return A Traversable of the edges as represented in the graph
   */
  override def innerEdges:Iterable[InnerEdgeType] = inSuccessors.flatten

  /**
   * O(n&#94;2)
   *
   * @return All of the edges in the graph
   */
  override def edges: Iterable[OuterEdgeType] = outSuccessors.flatten

  /**
   * O(n)
   *
   * @return the edge between start and end or noEdgeExistsValue
   */
  //todo reconsile with edge() and the other label.
  override def label(from: InnerNode, to: InnerNode):Label = {
    val indexedSet = inSuccessors(from.index).filter(x => x.to == to)
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next().label
      case _ => throw new IllegalStateException(s"Multiple edges from $from to $to: "+indexedSet)
    }
  }

  /**
   * O(1)
   *
   * @return
   */
  override def node(i: Int): Node = outNodes.apply(i)

  /**
   * O(1)
   *
   * @return
   */
  override def innerNodeForIndex(i: Int): InnerNode = innerNodes.apply(i)

  /**
   * O(n)
   *
   * @return
   */
  override def label(i: Int, j: Int): Label = {
    val indexedSet = inSuccessors(i).filter(x => x.to == inNodes.apply(j))
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next().label
      case _ => throw new IllegalStateException(s"Multiple edges from ${node(i)} to ${node(j)}: "+indexedSet)
    }
  }

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes,noEdgeExistsValue = $noEdgeExistsLabel)"
  }

  override def edge(from: InnerNode, to: InnerNode): Option[InnerEdgeType] = {

    val indexedSet = inSuccessors(from.index).filter(x => x.to == to)

    indexedSet.size match {
      case 0 => None
      case 1 => Some(indexedSet.iterator.next())
      case _ => throw new IllegalStateException(s"Multiple edges from $from to $to: "+indexedSet)
    }
  }
}

/**
 * O(n ln(n) + e ln(n))
 */
object AdjacencyLabelDigraph{

  def apply[Node,Label](edges:Iterable[(Node,Node,Label)] = Seq.empty,
                       nodes:Seq[Node] = Seq.empty,
                       noEdgeExistsValue:Label = null): AdjacencyLabelDigraph[Node, Label] = {

    val nodeValues: Vector[Node] = Vector.from((nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct)

    val successorMap:Map[Node,Iterable[(Node,Node,Label)]] = edges.groupBy(_._1)
    val predecessorMap:Map[Node,Iterable[(Node,Node,Label)]] = edges.groupBy(_._2)

    val successorAdjacencies:Vector[IndexedSet[(Node,Node,Label)]] = nodeValues.map(n => IndexedSet.from(successorMap.getOrElse(n,Vector.empty[(Node,Node,Label)])))
    val predecessorAdjacencies:Vector[IndexedSet[(Node,Node,Label)]] = nodeValues.map(n => IndexedSet.from(predecessorMap.getOrElse(n,Vector.empty[(Node,Node,Label)])))

    new AdjacencyLabelDigraph(IndexedSet.from(nodeValues),successorAdjacencies,predecessorAdjacencies,noEdgeExistsValue)
  }

}