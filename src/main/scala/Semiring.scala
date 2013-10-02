import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.{Graph => MutableGraph}

import scala.reflect.ClassTag
import scalax.collection.edge.LBase._

abstract class Semiring[Label: ClassTag] {

  object ImplicitLabel extends LEdgeImplicits[Label]
  import ImplicitLabel._

  //identity
  def I:Label
  //annihilator
  def O:Label

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Label,
              currentLabel:Label):Option[Label]

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Label,throughToLabel:Label):Label

  /**
   * Override this method if you need to work with nodes and edges directly as part of your summary operator.
   */
  def fullSummary[N](labelGraph:MutableGraph[N,LDiEdge])
                 (from:labelGraph.NodeT,
                  through:labelGraph.NodeT,
                  to:labelGraph.NodeT,
                  fromThroughToLabel:Label):Option[LDiEdge[N]] = {

    if(fromThroughToLabel != O) {
      val fromTo:Option[labelGraph.EdgeT] = from ~>? to
      val currentLabel:Label = fromTo match {
        case None => O
        case Some(edgeIn) => edgeIn.label
      }

      summary(fromThroughToLabel,currentLabel) match {
        case Some(labelUpdate) => Some((from.value ~+> to.value)(labelUpdate))
        case None => None
      }
    }
    else None
  }

  /**
   * Override this method if you need to work with nodes and edges directly as part of your extend operator.
   */
  def fullExtend[N](labelGraph:MutableGraph[N,LDiEdge])
                (fromThrough:Option[labelGraph.EdgeT],
                 throughTo:Option[labelGraph.EdgeT]):Label = {

    (fromThrough,throughTo) match {
      case (Some(fromThroughEdgeT),Some(throughToEdgeT)) => {
        val fromThroughLabel:Label = fromThrough.get.label
        val throughToLabel:Label = throughTo.get.label

        val fromToLabel:Label = extend(fromThroughLabel,throughToLabel)
        fromToLabel
      }
      case _ => O
    }
  }

  /**
   * Override this method to add side effects when you replace an edge.
   */
  def replaceEdge[N](labelGraph:MutableGraph[N,LDiEdge])
                 (from:labelGraph.NodeT,
                  to:labelGraph.NodeT,
                  replacementEdge:LDiEdge[N]):Unit = {

    from ~>? to match {
      case Some(oldEdge) => {
        labelGraph.remove(oldEdge.toEdgeIn)
      }
      case None => ;
    }
    labelGraph.addAndGet(replacementEdge)
  }

  /**
   * Override this method to add side effects to the relax operator
   */
  def relax[N](labelGraph:MutableGraph[N,LDiEdge])
           (from:labelGraph.NodeT,
            through:labelGraph.NodeT,
            to:labelGraph.NodeT):Unit = {

    val fromThrough:Option[labelGraph.EdgeT] = from ~>? through
    val throughTo:Option[labelGraph.EdgeT] = through ~>? to
    val fromThroughToLabel:Label = fullExtend(labelGraph)(fromThrough,throughTo)

    val fromToReplace:Option[LDiEdge[N]] = fullSummary(labelGraph)(from,through,to,fromThroughToLabel)
    fromToReplace match {
      //start here. replace the edge inside the summary operator
      case Some(replacementEdge) => {
        replaceEdge(labelGraph)(from,to,replacementEdge)
      }
      case None => ;
    }
  }
}

trait LabelGraphBuilder[Label] {

  def identityEdgeFromGraphNode[N](originalGraph:Graph[N,LDiEdge])
                                  (nodeT:originalGraph.NodeT)
                                  (semiring:Semiring[Label]):LDiEdge[N] = {
    val node:N = nodeT.value
    (node ~+> node)(semiring.I)
  }

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N]

  //todo when from starts using ClassTag or TypeTag, do the same. Graph.from uses a Manifest.
  def initialLabelGraph[N:Manifest](originalGraph:Graph[N,LDiEdge])
                                         (semiring:Semiring[Label]):MutableGraph[N,LDiEdge] = {
    import scala.collection.Set

    val nodes:Set[N] = originalGraph.nodes.toNodeInSet

    val identityLabelEdges:Set[LDiEdge[N]] = originalGraph.nodes.seq.map(identityEdgeFromGraphNode(originalGraph)(_)(semiring))
    val interestingLabelEdges:Set[LDiEdge[N]] = originalGraph.edges.seq.map(initialEdgeFromGraphEdge(originalGraph))
    val initEdges:Set[LDiEdge[N]] = identityLabelEdges ++ interestingLabelEdges
    MutableGraph.from(nodes,initEdges)
  }
}

/**
 * True if the sink can be reached from the source, false if not.
 */
object TransitiveClosureSemiring extends Semiring[Boolean] {

  //identity and annihilator
  def I = true
  def O = false

  def summary(fromThroughToLabel:Boolean,
              currentLabel:Boolean):Option[Boolean] = {
    if(fromThroughToLabel & !currentLabel) {
      Some(true)
    }
    else None
  }

  def extend(fromThroughLabel:Boolean,throughToLabel:Boolean):Boolean = {
    fromThroughLabel & throughToLabel
  }

}

object TransitiveClosureLabelGraphBuilder extends LabelGraphBuilder[Boolean] {

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn
    (edge._1 ~+> edge._2)(true)
  }
}


/**
 * Finds the length of a path that traverses the fewest nodes. This is about the simplest semiring I can come up with.
 */
object CountFewestNodesBetweenSemiring extends Semiring[Int] {

  //identity and annihilator
  def I = 0
  def O = Int.MaxValue

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Int,
              currentLabel:Int):Option[Int] = {
    if(fromThroughToLabel < currentLabel) {
      Some(fromThroughToLabel)
    }
    else None
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Int,throughToLabel:Int):Int = {
    fromThroughLabel + throughToLabel
  }
}

object CountFewestNodesBetweenGraphBuilder extends LabelGraphBuilder[Int] {

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn
    (edge._1 ~+> edge._2)(1)
  }
}

/**
 * Finds a path that traverses the fewest nodes. An edge is a list of the nodes to traverse in the original graph.
 */
class OneShortestPathSemiring[N] extends Semiring[Option[List[N]]] {

  //length of the path is length of the list

  //identity and annihilator
  def I = Some(List[N]())
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[List[N]],
              currentLabel:Option[List[N]]):Option[Option[List[N]]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToNodes),Some(currentNodes)) => {
        if(fromThroughToNodes.size < currentNodes.size) Some(fromThroughToLabel)
        else None
      }
      case (Some(fromThroughToNodes),None) => Some(fromThroughToLabel)
      case _ => None
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Option[List[N]],throughToLabel:Option[List[N]]):Option[List[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughNodes),Some(throughToNodes)) => {
        val fromToLabel:Option[List[N]] = Some(fromThroughNodes ++ throughToNodes)
        fromToLabel
      }
      case _ => None
    }
  }
}

class OneShortestPathGraphBuilder[N] extends LabelGraphBuilder[Option[List[N]]] {

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn
    (edge._1 ~+> edge._2)(Some(List(edge._2)))
  }
}

/**
 * Finds all path that traverses the fewest nodes.
 */

case class NextStep[N](steps:Int,choices:Set[N]) {}

class AllShortestPathsSemiring[N] extends Semiring[Option[NextStep[N]]] {

  //length of the path is length of the list

  //identity and annihilator
  def I = Some(NextStep[N](0,Set[N]()))
  def O = None

  /**
   * Implement this method to create the core of a summary operator
   */
  def summary(fromThroughToLabel:Option[NextStep[N]],
              currentLabel:Option[NextStep[N]]):Option[Option[NextStep[N]]] = {

    (fromThroughToLabel,currentLabel) match {
      case (Some(fromThroughToSteps),Some(currentSteps)) => {
        if(fromThroughToSteps.steps < currentSteps.steps) Some(fromThroughToLabel)
        else if (fromThroughToSteps.steps == currentSteps.steps) {
          Some(Some(new NextStep[N](currentSteps.steps,currentSteps.choices ++ fromThroughToSteps.choices)))
        }
        else None
      }
      case (Some(fromThroughToNodes),None) => Some(fromThroughToLabel)
      //the currentLabel is already in the graph
      case _ => None
    }
  }

  /**
   * Implement this method to create the core of an extend operator
   */
  def extend(fromThroughLabel:Option[NextStep[N]],throughToLabel:Option[NextStep[N]]):Option[NextStep[N]] = {

    (fromThroughLabel,throughToLabel) match {
      case (Some(fromThroughSteps),Some(throughToSteps)) => {
        Some(new NextStep[N](fromThroughSteps.steps+throughToSteps.steps,fromThroughSteps.choices))
      }
      case _ => None
    }
  }
}

class AllShortestPathsGraphBuilder[N] extends LabelGraphBuilder[Option[NextStep[N]]] {

  def initialEdgeFromGraphEdge[N](originalGraph:Graph[N,LDiEdge])
                                 (edgeT:originalGraph.EdgeT):LDiEdge[N] = {
    val edge:LDiEdge[N] = edgeT.toEdgeIn
    (edge._1 ~+> edge._2)(Some(new NextStep(1,Set[N](edge._2))))
  }
}

object FloydWarshall {

  def floydWarshall[N,Label](labelGraph:MutableGraph[N,LDiEdge])(semiring:Semiring[Label]):Unit = {
    val nodeTs = labelGraph.nodes
    for (k <- nodeTs; i <- nodeTs; j <- nodeTs) {
      semiring.relax(labelGraph)(i,k,j)
    }
  }

  def allPairsShortestPaths[N:Manifest,Label](originalGraph:Graph[N,LDiEdge])(semiring:Semiring[Label])(labelGraphBuilder:LabelGraphBuilder[Label]):Graph[N,LDiEdge] = {
    val labelGraph:MutableGraph[N,LDiEdge] = labelGraphBuilder.initialLabelGraph(originalGraph)(semiring)
    floydWarshall(labelGraph)(semiring)
    labelGraph
  }
}

object SomeGraph {

   //boring example graph 

  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"

  val nodes = List(A,B,C,D,E,F,G,H)

  val ab = (A~+>B)("ab")
  val bc = (B~+>C)("bc")
  val cd = (C~+>D)("cd")
  val de = (D~+>E)("de")
  val ef = (E~+>F)("ef")
  val eb = (E~+>B)("eb")
  val eh = (E~+>H)("eh")
  val hc = (H~+>C)("hc")

  val edges = List(ab,bc,cd,de,ef,eb,eh,hc)

  val graph:Graph[String,LDiEdge] = Graph.from(nodes,edges)

}
