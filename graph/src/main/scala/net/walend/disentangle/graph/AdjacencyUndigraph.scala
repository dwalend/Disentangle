package net.walend.disentangle.graph

import scala.collection.{GenMap, GenSeq, GenTraversable}

/**
  * Provides constant-time access for edges of a node.
  *
  * The constructor is O(n + a ln(n))
  *
  * @author dwalend
  * @since v0.2.1
  */
class AdjacencyUndigraph[Node](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                          outEdges:Vector[IndexedSet[NodePair[Node]]] // (i) is the edges for node i, (j) is the NodePair[node,node] pair to reach that second node.
                                         ) extends IndexedUndigraph[Node] {

  val inNodes:IndexedSet[InNode] =outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  //todo really should be a Set, not an IndexedSet
  def neighborSet(indexedSet:IndexedSet[OuterEdgeType]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(e => NodePair(nodeToInNode(e._1),nodeToInNode(e._2)))
  }

  //todo really should be a Set, not an IndexedSet
  val inEdges:Vector[IndexedSet[InnerEdgeType]] = outEdges.map(neighborSet)

  def nodes = outNodes

  override def nodeCount: Int = outNodes.size

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.InnerIndexedNodeTrait {

    override def edges: IndexedSet[InnerEdgeType] = {
      inEdges(index)
    }

    override def hashCode(): Int = index

    override def equals(thing: Any): Boolean = {
      thing match {
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
  override def innerNodes: IndexedSet[InNode] = inNodes

  /**
    * @return A Traversable of the edges as represented in the graph
    */
  override def innerEdges:Vector[InnerEdgeType] = inEdges.flatten

  /**
    * O(n&#94;2)
    *
    * @return All of the edges in the graph
    */
  override def edges: Seq[OuterEdgeType] = outEdges.flatten

  /**
    * O(1)
    *
    * @return
    */
  override def node(i: Int): Node = outNodes.get(i)

  /**
    * O(1)
    *
    * @return
    */
  override def innerNodeForIndex(i: Int): InNode = innerNodes.get(i)

  override def toString:String = {
    s"${this.getClass.getSimpleName}(edges = $edges,nodes = $outNodes)"
  }
}

object AdjacencyUndigraph{

  //noinspection ConvertibleToMethodValue
  def apply[Node](edges:GenTraversable[NodePair[Node]] = Seq.empty,
                        nodes:GenSeq[Node] = Seq.empty) = {

    val nodeValues:Vector[Node] = (nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct.to[Vector]

    val successorMap = edges.groupBy(x => x._1)
    val predecessorMap = edges.groupBy(x => x._2)

    def getOrEmpty(n:Node,nodeToTrav:GenMap[Node,GenTraversable[NodePair[Node]]]) = {
      nodeToTrav.getOrElse(n,Vector.empty[NodePair[Node]]).to[IndexedSet]
    }

    val edgeAdjacencies = nodeValues.map(n => getOrEmpty(n,successorMap) ++ getOrEmpty(n,predecessorMap) )

    new AdjacencyUndigraph(nodeValues.to[IndexedSet],edgeAdjacencies)
  }

}

/**
  * A digraph that exposes the indices of stored nodes.
  */
trait IndexedUndigraph[Node] extends Undigraph[Node] {

  /**
    * The type of InnerNodeTrait for this digraph representation
    */
  type InnerNodeType <: InnerIndexedNodeTrait

  override type OuterEdgeType = NodePair[Node]
  override type InnerEdgeType = NodePair[InnerNodeType]

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