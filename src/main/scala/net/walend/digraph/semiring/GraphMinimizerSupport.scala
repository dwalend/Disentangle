package net.walend.digraph.semiring

import net.walend.scalagraph.minimizer.heap.HeapOrdering

/**
 * Parts for semiring-based graph minimizing algorithms.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait GraphMinimizerSupport[L,Key] {

  type Label = L

  def semiring:Semiring

  def heapOrdering:HeapOrdering[Key]

  def heapKeyForLabel:Label => Key

  trait Semiring {

    /** identity */
    def I:Label
    /** annihilator */
    def O:Label

    /**
     * true if the value is within the Semiring's domain
     */
    def inDomain(label:Label):Boolean

    /**
     * Implement this method to create the core of a summary operator
     */
    def summary(fromThroughToLabel:Label,currentLabel:Label):Label

    /**
     * Implement this method to create the core of an extend operator
     */
    def extend(fromThroughLabel:Label,throughToLabel:Label):Label

    /**
     * Override this method to add side effects to the relax operator
     */
    def relax[N](labelGraph:Digraph[N,Label])
                (from:labelGraph.InnerNodeType,
                 through:labelGraph.InnerNodeType,
                 to:labelGraph.InnerNodeType):Label = {

      val fromThrough:Label = labelGraph.edge(from,through)
      val throughTo:Label = labelGraph.edge(through,to)
      val fromThroughTo:Label = extend(fromThrough,throughTo)

      val current:Label = labelGraph.edge(from,to)

      val summaryLabel:Label = summary(fromThroughTo,current)

      labelGraph.updateEdge(from,to,summaryLabel)

      summaryLabel
    }
  }
}

trait GraphMinimizerSupportWithGraphConverter[L,Key] extends GraphMinimizerSupport[L,Key] {

  //todo factor this out into its own trait
  def convertEdgeToLabel[Node,Edge](start:Node,end:Node,edge:Edge):Label

  def convertGraph[Node,Edge](graph:Digraph[Node,Edge],labelForEdge:(Node,Node,Edge)=>Label):Digraph[Node,Label] = {

    val nodes = graph.nodes
    val edges = graph.nodes.map(x => (x,x,semiring.I)) ++
      graph.edges.map(x => (x._1,x._2,labelForEdge(x._1,x._2,x._3)))

    FastDigraph(edges,nodes,semiring.O)
  }

  //todo this kinda becomes its own thing, only in semirings where appropriate
  def convertGraph[Node,Edge](graph:Digraph[Node,Edge]):Digraph[Node,Label] = convertGraph(graph,convertEdgeToLabel)

//todo this is what the algorithms should take in, a function that creates a graph from anything the user wants  def createLabelGraph[Node,Label](body: ⇒ Digraph[Node,Label]):Digraph[Node,Label]
}