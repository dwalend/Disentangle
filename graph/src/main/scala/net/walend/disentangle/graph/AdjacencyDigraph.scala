package net.walend.disentangle.graph

import scala.collection.{GenMap, GenSeq, GenTraversable}

/**
  * Provides constant-time access for successor and predecessor edges of a node.
  *
  * The constructor is O(n + a ln(n))
  *
  * @author dwalend
  * @since v0.2.1
  */
class AdjacencyDigraph[Node](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                        outSuccessors:Vector[IndexedSet[(Node,Node)]], // (i) is the successors for node i, (j) is the node,node tuple to reach that second node.
                                        outPredecessors:Vector[IndexedSet[(Node,Node)]]
                                       ) extends Tuple2Digraph[Node] with IndexedGraph[Node] {

  val inNodes:IndexedSet[InNode] =outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborSet(indexedSet:IndexedSet[(Node,Node)]):IndexedSet[(InNode,InNode)] = {
    indexedSet.map(x => (nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get))
  }

  val inSuccessors:Vector[IndexedSet[(InNode,InNode)]] = outSuccessors.map(neighborSet)
  val inPredecessors:Vector[IndexedSet[(InNode,InNode)]] = outPredecessors.map(neighborSet)

  def nodes = outNodes

  override def nodeCount: Int = outNodes.size

  type InnerNodeType = InNode

  case class InNode(override val value:Node,override val index:Int) extends this.DigraphInnerNodeTrait with this.InnerIndexedNodeTrait {

    override def successors: IndexedSet[(InNode,InNode)] = {
      inSuccessors(index)
    }

    override def predecessors: IndexedSet[(InNode,InNode)] = {
      inPredecessors(index)
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

  override type InnerEdgeType = (InNode,InNode)

  /**
    * @return A Traversable of the edges as represented in the graph
    */
  override def innerEdges:Vector[InnerEdgeType] = inSuccessors.flatten

  /**
    * O(n&#94;2)
    *
    * @return All of the edges in the graph
    */
  override def edges: Seq[OuterEdgeType] = outSuccessors.flatten

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

  override def edge(from: InNode, to: InNode): Option[(InNode, InNode)] = ???  //todo
}

/**
  * O(n ln(n) + e ln(n))
  */
object AdjacencyDigraph{

  def apply[Node](edges:GenTraversable[(Node,Node)] = Seq.empty,
                        nodes:GenSeq[Node] = Seq.empty) = {

    val nodeValues:Vector[Node] = (nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct.to[Vector]

    val successorMap:GenMap[Node,GenTraversable[(Node,Node)]] = edges.groupBy(_._1)
    val predecessorMap:GenMap[Node,GenTraversable[(Node,Node)]] = edges.groupBy(_._2)

    val successorAdjacencies:Vector[IndexedSet[(Node,Node)]] = nodeValues.map(n => successorMap.getOrElse(n,Vector.empty[(Node,Node)]).to[IndexedSet])
    val predecessorAdjacencies:Vector[IndexedSet[(Node,Node)]] = nodeValues.map(n => predecessorMap.getOrElse(n,Vector.empty[(Node,Node)]).to[IndexedSet])

    new AdjacencyDigraph(nodeValues.to[IndexedSet],successorAdjacencies,predecessorAdjacencies)
  }

}
