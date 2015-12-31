package net.walend.disentangle.graph

import scala.collection.immutable.Set.Set2
import scala.collection.{GenMap, GenSeq, GenTraversable}

/**
  * Provides constant-time access for edges of a node.
  *
  * The constructor is O(n + a ln(n))
  *
  * @author dwalend
  * @since v0.2.1
  */
class AdjacencyLabelUndigraph[Node,Label](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                          outEdges:Vector[IndexedSet[(Set2[Node],Label)]], // (i) is the edges for node i, (j) is the Set2[node,node],edge pair to reach that second node.
                                        val noEdgeExistsLabel:Label //value for no edge
                                       ) extends IndexedLabelUndigraph[Node,Label] {

  val inNodes:IndexedSet[InNode] =outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  //todo really should be a Set, not an IndexedSet
  def neighborSet(indexedSet:IndexedSet[OuterEdgeType]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(x => (x._1.map(n => nodeToInNode(n)).asInstanceOf[Set2[InnerNodeType]],x._2))
  }

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

  override type InnerEdgeType = (Set2[InNode],Label)

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
    * O(n)
    *
    * @return the edge between start and end or noEdgeExistsValue
    */
  override def label(between:Set2[InnerNodeType]):Label = {

    val (from,to) = {
      val it = between.iterator
      (it.next(),it.next())
    }

    val indexedSet = inEdges(from.index).filter(x => x._1.contains(to))
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next()._2
      case _ => throw new IllegalStateException(s"Multiple edges from $from to $to: "+indexedSet)
    }
  }

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

  /**
    * O(n)
    *
    * @return
    */
  override def label(i: Int, j: Int): Label = {
    val indexedSet = inEdges(i).filter(x => x._2 == inNodes.get(j))
    indexedSet.size match {
      case 0 => noEdgeExistsLabel
      case 1 => indexedSet.iterator.next()._2
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

  def apply[Node,Label](edges:GenTraversable[(Set2[Node],Label)] = Seq.empty,
                        nodes:GenSeq[Node] = Seq.empty,
                        noEdgeExistsValue:Label = null) = {

    val nodeValues:Vector[Node] = (nodes ++ edges.flatMap(_._1)).distinct.to[Vector]

    def first(s:Set2[Node]):Node = s.iterator.next()

    def second(s:Set2[Node]):Node = {
      val it = s.iterator
      it.next()
      it.next()
    }

    val successorMap:GenMap[Node,GenTraversable[(Set2[Node],Label)]] = edges.groupBy(x => first(x._1))
    val predecessorMap:GenMap[Node,GenTraversable[(Set2[Node],Label)]] = edges.groupBy(x => second(x._1))

    def getOrEmpty(n:Node,nodeToTrav:GenMap[Node,GenTraversable[(Set2[Node],Label)]]):IndexedSet[(Set2[Node],Label)] = {
      nodeToTrav.getOrElse(n,Vector.empty[(Set2[Node],Label)]).to[IndexedSet]
    }

    val edgeAdjacencies:Vector[IndexedSet[(Set2[Node],Label)]] = nodeValues.map(n => (getOrEmpty(n,successorMap) ++ getOrEmpty(n,predecessorMap)) )

    new AdjacencyLabelUndigraph(nodeValues.to[IndexedSet],edgeAdjacencies,noEdgeExistsValue)
  }

}