package net.walend.disentangle.graph

import scala.collection.immutable.{Map, Seq, Iterable}

/**
  * Provides constant-time access for edges of a node.
  *
  * The constructor is O(n + a ln(n))
  *
  * @author dwalend
  * @since v0.2.1
  */
class AdjacencyLabelUndigraph[Node,Label](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                          outEdges:Vector[IndexedSet[(NodePair[Node],Label)]], // (i) is the edges for node i, (j) is the NodePair[node,node],edge pair to reach that second node.
                                          val noEdgeExistsLabel:Label //value for no edge
                                       ) extends IndexedLabelUndigraph[Node,Label] {

  type InnerEdgeType = InnerEdge
  case class InnerEdge(nodePair: NodePair[InNode],label: Label) extends UndigraphInnerEdgeTrait {
    override def value: (NodePair[Node],Label) = (NodePair(nodePair._1.value,nodePair._2.value),label)
  }
  object InnerEdge{
    def apply(_1:InNode, _2:InNode, label: Label): InnerEdge = new InnerEdge(NodePair(_1,_2),label)
  }

  type InnerNodeType = InNode
  case class InNode(override val value:Node,override val index:Int) extends this.UndigraphInnerNodeTrait with this.InnerIndexedNodeTrait {

    override def innerEdges: IndexedSet[InnerEdgeType] = {
      inEdges(index)
    }

    override def outerEdges: Set[(NodePair[Node], Label)] = {
      outEdges(index)
    }

    override def hashCode(): Int = index

    override def equals(thing: Any): Boolean = {
      thing match {
        case inNode:InNode => inNode.index == index
        case _ => false
      }
    }
  }

  val inNodes:IndexedSet[InNode] =outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  //todo really should be a Set, not an IndexedSet
  def neighborSet(indexedSet:IndexedSet[OuterEdgeType]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(e => InnerEdge(NodePair(nodeToInNode(e._1._1),nodeToInNode(e._1._2)),e._2))
  }

  //todo really should be a Set, not an IndexedSet
  val inEdges:Vector[IndexedSet[InnerEdgeType]] = outEdges.map(neighborSet)

  def nodes: IndexedSet[Node] = outNodes

  override def nodeCount: Int = outNodes.size

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
  override def innerNodes: IndexedSet[InNode] = inNodes

  /**
    * @return A Traversable of the edges as represented in the graph
    */
  override lazy val innerEdges:Vector[InnerEdgeType] = inEdges.flatten.distinct

  /**
    * O(n&#94;2)
    *
    * @return All of the edges in the graph
    */
  override def edges: Seq[OuterEdgeType] = outEdges.flatten.distinct

  /**
    * O(n)
    *
    * @return the edge between start and end or noEdgeExistsValue
    */
  override def label(between:NodePair[InnerNodeType]):Label = {

    val indexedSet = inEdges(between._1.index).filter(x => x.nodePair.contains(between._2))
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next().label
      case _ => throw new IllegalStateException(s"Multiple edges between $between: "+indexedSet)
    }
  }


  /**
    * @return the Label between a pair of nodes, or noEdgeExistsLable if no edge exists.
    * @throws IllegalArgumentException if either node is not in the graph
    */
  override def edge(between: _root_.net.walend.disentangle.graph.NodePair[Node]): InnerEdgeType = {
    val a: InNode = innerNode(between._1).getOrElse(throw new IllegalArgumentException(s"${between._1} is not in $this"))
    val b: InNode = innerNode(between._2).getOrElse(throw new IllegalArgumentException(s"${between._2} is not in $this"))
    val nodePair = NodePair(a,b)
    InnerEdge(nodePair,label(nodePair))
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
  override def innerNodeForIndex(i: Int): InNode = innerNodes.apply(i)

  /**
    * O(n)
    *
    * @return
    */
  override def label(i: Int, j: Int): Label = {
    val indexedSet = inEdges(i).filter(x => x.nodePair._2 == inNodes.apply(j))
    indexedSet.size match {
      case 1 => indexedSet.iterator.next().label
      case 0 => noEdgeExistsLabel
      case _ => throw new IllegalStateException(s"Multiple edges from ${node(i)} to ${node(j)}: "+indexedSet)
    }
  }

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes,noEdgeExistsValue = $noEdgeExistsLabel)"
  }
}

/**
  * O(n ln(n) + e ln(n))
  */
object AdjacencyLabelUndigraph{

  //noinspection ConvertibleToMethodValue
  def apply[Node,Label](edges:Iterable[(NodePair[Node],Label)] = Seq.empty,
                        nodes:Seq[Node] = Seq.empty,
                        noEdgeExists:Label = null): AdjacencyLabelUndigraph[Node, Label] = {

    val nodeValues = Vector.from((nodes ++ edges.map(_._1._1) ++ edges.map(_._1._2)).distinct)

    val successorMap:Map[Node,Iterable[(NodePair[Node],Label)]] = edges.groupBy(x => x._1._1)
    val predecessorMap:Map[Node,Iterable[(NodePair[Node],Label)]] = edges.groupBy(x => x._1._2)

    def getOrEmpty(n:Node,nodeToTrav:Map[Node,Iterable[(NodePair[Node],Label)]]):IndexedSet[(NodePair[Node],Label)] = {
      IndexedSet.from(nodeToTrav.getOrElse(n,Vector.empty[(NodePair[Node],Label)]))
    }

    val edgeAdjacencies: Vector[IndexedSet[(NodePair[Node], Label)]] = nodeValues.map(n => getOrEmpty(n,successorMap) ++ getOrEmpty(n,predecessorMap) )

    new AdjacencyLabelUndigraph(IndexedSet.from(nodeValues),edgeAdjacencies,noEdgeExists)
  }

}