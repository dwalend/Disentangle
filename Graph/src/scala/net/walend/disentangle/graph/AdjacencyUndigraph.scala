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
case class AdjacencyUndigraph[Node](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                    adjacencyMatrix:Vector[IndexedSet[NodePair[Node]]] // (i) is the edges for node i, (j) is the NodePair[node,node] pair to reach that second node.
                                         ) extends IndexedUndigraph[Node] {

  type InnerNodeType = InNode
  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def innerEdges: IndexedSet[InnerEdgeType] = {
      inAdjacencyMatrix(index)
    }

    override def outerEdges: Set[NodePair[Node]] = {
      adjacencyMatrix(index)
    }

    override def hashCode(): Int = index

    override def equals(thing: Any): Boolean = {
      thing match {
        case inNode:InNode => inNode.index == index
        case _ => false
      }
    }
  }

  type InnerEdgeType = InnerEdge
  case class InnerEdge(nodePair: NodePair[InNode]) extends UndigraphInnerEdgeTrait {
    override def value: NodePair[Node] = NodePair(nodePair._1.value,nodePair._2.value)
  }
  object InnerEdge{
    def apply(_1:InNode,_2:InNode): InnerEdge = new InnerEdge(NodePair(_1,_2))
  }

  val inNodes:IndexedSet[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  //todo really should be a Set, not an IndexedSet
  def neighborSet(indexedSet:IndexedSet[OuterEdgeType]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(e => InnerEdge(nodeToInNode(e._1),nodeToInNode(e._2)))
  }

  //todo really should be a Set, not an IndexedSet
  val inAdjacencyMatrix:Vector[IndexedSet[InnerEdgeType]] = adjacencyMatrix.map(neighborSet)

  def nodes = outNodes

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
  override lazy val innerEdges:Vector[InnerEdgeType] = inAdjacencyMatrix.flatten.distinct

  /**
    * O(n&#94;2)
    *
    * @return All of the edges in the graph
    */
  override lazy val edges: Seq[OuterEdgeType] = adjacencyMatrix.flatten.distinct

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

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes)"
  }

  override def equals(that: Any): Boolean =
    that match {

      case that: Graph[_] =>
        (edges.toSet.equals(that.edges.toSet)) && (nodes.equals(that.nodes))

      case _ => false
    }

  override lazy val hashCode:Int = edges.toSet.hashCode() + nodes.hashCode()

}

object AdjacencyUndigraph{

  //noinspection ConvertibleToMethodValue
  def apply[Node](edges:Iterable[NodePair[Node]] = Seq.empty,
                        nodes:Seq[Node] = Seq.empty): AdjacencyUndigraph[Node] = {

    val nodeValues = Vector.from((nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct)

    val successorMap = edges.groupBy(x => x._1)
    val predecessorMap = edges.groupBy(x => x._2)

    def getOrEmpty(n:Node,nodeToTrav:Map[Node,Iterable[NodePair[Node]]]) = {
      IndexedSet.from(nodeToTrav.getOrElse(n,Vector.empty[NodePair[Node]]))
    }

    val edgeAdjacencies: Vector[IndexedSet[NodePair[Node]]] = nodeValues.map(n => getOrEmpty(n,successorMap) ++ getOrEmpty(n,predecessorMap) )

    new AdjacencyUndigraph(IndexedSet.from(nodeValues),edgeAdjacencies)
  }

  def fromPairs[Node](edges:Iterable[(Node,Node)],
                      nodes:Seq[Node] = Seq.empty): AdjacencyUndigraph[Node] = {
    apply(edges.map(x => NodePair(x._1,x._2)),nodes)
  }

}

/**
  * A digraph that exposes the indices of stored nodes.
  */
trait IndexedUndigraph[Node] extends Undigraph[Node] {

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  override type InnerNodeType <: InnerIndexedNodeTrait

  override type OuterEdgeType = NodePair[Node]

  /**
    * All the nodes in the graph, in an indexed set
    */
  def nodes:IndexedSet[Node]

  /**
    * @return internal representation of all of the nodes in the graph.
    */
  def innerNodes:IndexedSet[InnerNodeType]

  /**
    * An internal representation of nodes within the graph
    */
  trait InnerIndexedNodeTrait extends UndigraphInnerNodeTrait {

    def index:Int
  }

  def node(i:Int):Node

  def innerNodeForIndex(i:Int):InnerNodeType

//todo if needed, and maybe one for the nodes, too  def exists(i:Int,j:Int):Boolean
}