package net.walend.disentangle.graph

import scala.collection.{Map, Seq, Iterable}

/**
  * Provides constant-time access for successor and predecessor edges of a node.
  *
  * The constructor is O(n + a ln(n))
  *
  * @author dwalend
  * @since v0.2.1
  */
class AdjacencyDigraph[Node](outNodes:IndexedSet[Node], //provides the master index values for each node.
                                        outSuccessors:IndexedSet[IndexedSet[(Node,Node)]], // (i) is the successors for node i, (j) is the node,node tuple to reach that second node.
                                        outPredecessors:IndexedSet[IndexedSet[(Node,Node)]]
                                       ) extends Tuple2Digraph[Node] with IndexedGraph[Node] {
  override type InnerNodeType = InNode
  case class InNode(override val value:Node,override val index:Int) extends this.DigraphInnerNodeTrait with this.InnerIndexedNodeTrait {

    override def successors: IndexedSet[InnerEdgeType] = {
      inSuccessors.apply(index)
    }

    override def predecessors: IndexedSet[InnerEdgeType] = {
      inPredecessors.apply(index)
    }

    override def hashCode(): Int = index

    override def equals(thing: Any): Boolean = {
      thing match {
        case inNode:InNode => inNode.index == index
        case _ => false
      }
    }
  }

  override type InnerEdgeType = InnerEdge
  case class InnerEdge(from:InNode,to:InNode) extends DigraphInnerEdgeTrait {
    override def value: OuterEdgeType = (from.value,to.value)
  }

  val inNodes:IndexedSet[InNode] = outNodes.zipWithIndex.map(x => InNode(x._1,x._2))
  val nodeToInNode:Map[Node,InNode] = inNodes.map(x => x.value -> x).toMap

  def neighborSet(indexedSet:IndexedSet[(Node,Node)]):IndexedSet[InnerEdgeType] = {
    indexedSet.map(x => InnerEdge(nodeToInNode.get(x._1).get,nodeToInNode.get(x._2).get))
  }

  val inSuccessors:IndexedSet[IndexedSet[InnerEdgeType]] = outSuccessors.map(neighborSet)
  val inPredecessors:IndexedSet[IndexedSet[InnerEdgeType]] = outPredecessors.map(neighborSet)

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
  override def innerEdges:IndexedSet[InnerEdgeType] = inSuccessors.flatten

  /**
    * O(n&#94;2)
    *
    * @return All of the edges in the graph
    */
  override def edges: IndexedSet[OuterEdgeType] = outSuccessors.flatten

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

  override def edge(from: InNode, to: InNode): Option[InnerEdgeType] = ???  //todo
}

/**
  * O(n ln(n) + e ln(n))
  */
object AdjacencyDigraph{

  def apply[Node](edges:Iterable[(Node,Node)] = Seq.empty,
                        nodes:Seq[Node] = Seq.empty) = {

    val nodeValues = IndexedSet.from((nodes ++ edges.map(_._1) ++ edges.map(_._2)).distinct)

    val successorMap:Map[Node,Iterable[(Node,Node)]] = edges.groupBy(_._1)
    val predecessorMap:Map[Node,Iterable[(Node,Node)]] = edges.groupBy(_._2)

    val successorAdjacencies: IndexedSet[IndexedSet[(Node, Node)]] = nodeValues.map(n => IndexedSet.from(successorMap.getOrElse(n,Vector.empty[(Node,Node)])))
    val predecessorAdjacencies: IndexedSet[IndexedSet[(Node, Node)]] = nodeValues.map(n => IndexedSet.from(predecessorMap.getOrElse(n,Vector.empty[(Node,Node)])))

    new AdjacencyDigraph(nodeValues,successorAdjacencies,predecessorAdjacencies)
  }

}
