package walend.scalax.semiring

/**
 *
 *
 * @author dwalend
 * @since v1
 */
import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.Graph
import LDiEdge._

import walend.scalax.gengraph.GraphFactory

class AllShortestPathsPredecessorsTest extends FlatSpec with Matchers {

  val expectedEdges = Set(
    (B~+>B)(Some(PrevStep(0,Set(),0))),
    (B~+>C)(Some(PrevStep(1,Set(B),1))),
    (B~+>D)(Some(PrevStep(2,Set(C),1))),
    (B~+>E)(Some(PrevStep(3,Set(D),1))),
    (B~+>F)(Some(PrevStep(4,Set(E),1))),
    (B~+>H)(Some(PrevStep(4,Set(E),1))),
    (F~+>F)(Some(PrevStep(0,Set(),0))),
    (C~+>C)(Some(PrevStep(0,Set(),0))),
    (C~+>D)(Some(PrevStep(1,Set(C),1))),
    (C~+>E)(Some(PrevStep(2,Set(D),1))),
    (C~+>F)(Some(PrevStep(3,Set(E),1))),
    (C~+>B)(Some(PrevStep(3,Set(E),1))),
    (C~+>H)(Some(PrevStep(3,Set(E),1))),
    (G~+>G)(Some(PrevStep(0,Set(),0))),
    (D~+>D)(Some(PrevStep(0,Set(),0))),
    (D~+>E)(Some(PrevStep(1,Set(D),1))),
    (D~+>F)(Some(PrevStep(2,Set(E),1))),
    (D~+>B)(Some(PrevStep(2,Set(E),1))),
    (D~+>H)(Some(PrevStep(2,Set(E),1))),
    (D~+>C)(Some(PrevStep(3,Set(H, B),2))),
    (H~+>H)(Some(PrevStep(0,Set(),0))),
    (H~+>C)(Some(PrevStep(1,Set(H),1))),
    (H~+>D)(Some(PrevStep(2,Set(C),1))),
    (H~+>E)(Some(PrevStep(3,Set(D),1))),
    (H~+>F)(Some(PrevStep(4,Set(E),1))),
    (H~+>B)(Some(PrevStep(4,Set(E),1))),
    (E~+>E)(Some(PrevStep(0,Set(),0))),
    (E~+>F)(Some(PrevStep(1,Set(E),1))),
    (E~+>B)(Some(PrevStep(1,Set(E),1))),
    (E~+>H)(Some(PrevStep(1,Set(E),1))),
    (E~+>C)(Some(PrevStep(2,Set(B, H),2))),
    (E~+>D)(Some(PrevStep(3,Set(C),2))),
    (A~+>A)(Some(PrevStep(0,Set(),0))),
    (A~+>B)(Some(PrevStep(1,Set(A),1))),
    (A~+>C)(Some(PrevStep(2,Set(B),1))),
    (A~+>D)(Some(PrevStep(3,Set(C),1))),
    (A~+>E)(Some(PrevStep(4,Set(D),1))),
    (A~+>F)(Some(PrevStep(5,Set(E),1))),
    (A~+>H)(Some(PrevStep(5,Set(E),1))))

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph
    val allShortestPathsSemiring = new AllShortestPathsPredecessorsSemiring[String](true)

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPathsSemiring)(new AllShortestPathsPredecessorsGraphBuilder[String])

    val foundEdges:Set[LDiEdge[String]] = labelGraph.edges.map(PrevStep.previousStepEdgeToPrevStepEdge(labelGraph)).flatten.to[Set]
    (foundEdges -- expectedEdges) should be (Set.empty)
    foundEdges should be (expectedEdges)
  }

  def timeFloyd(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val allShortestPaths = new AllShortestPathsPredecessors[Int]

    val startTime = System.currentTimeMillis()
    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsPredecessorsGraphBuilder[Int])
    val time = System.currentTimeMillis() - startTime

    val expected:Long = ((Math.pow(nodeCount.toDouble/calibrate._1,3) ) * calibrate._2).toLong
    println("nodeCount:"+nodeCount+" actual:"+time+" expected:"+expected)
    (nodeCount,time,expected)
  }
/*
  "The Floyd-Warshall algorithm" should "scale up at  O(|V|^3)" in {

    //warm up
    timeFloyd(32,(1,1,1))

    val calibrate = timeFloyd(32,(1,1,1))

    val result = (5.0.to(6.0,0.5)).map(x => timeFloyd(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
*/
  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph
    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPaths,new AllShortestPathsPredecessorsGraphBuilder[String])

    val edges:Set[LDiEdge[String]] = labelGraph.edges.map(PrevStep.previousStepEdgeToPrevStepEdge(labelGraph)).flatten.to[Set]
    (edges -- expectedEdges) should be (Set.empty)
    edges should be (expectedEdges)
  }

  def timeDijkstra(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
//todo switch back to the right semiring    val allShortestPaths = new AllShortestPathsPredecessors[Int]
    val allShortestPaths = new AllShortestPaths[Int]

    val startTime = System.currentTimeMillis()
    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPaths,new AllShortestPathsGraphBuilder[Int])
    val time = System.currentTimeMillis() - startTime

    val calibrateBigO = Math.pow(calibrate._1,2) * Math.log(calibrate._1)
    val constant = calibrate._2 / calibrateBigO

    val bigO =  Math.pow(nodeCount,2) * Math.log(nodeCount)

    val expected:Long = (constant * bigO).toLong
    println("nodeCount:"+nodeCount+" actual:"+time+" expected:"+expected)
    (nodeCount,time,expected)
  }
/*
  "The Dijkstra algorithm" should "scale up at  O(|V|^2 ln|V|)" in {

    //warm up
    timeDijkstra(32,(1,1,1))

    val calibrate = timeDijkstra(32,(1,1,1))

//    val result = (5.0.to(8.0,0.5)).map(x => timeDijkstra(Math.pow(2,x).toInt,calibrate))
    val result = (5.0.to(7.0,0.5)).map(x => timeDijkstra(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
*/
  "Brandes' algorithm" should "produce the correct betweenness for SomeGraph" in {

    val graph = SomeGraph.testGraph
    val allShortestPaths = new AllShortestPathsPredecessors[String]

    val labelGraphAndBetweenness = Brandes.shortestPathsAndBetweenness(graph)(allShortestPaths,new AllShortestPathsPredecessorsGraphBuilder[String])
    val labelGraph = labelGraphAndBetweenness._1

    val foundEdges:Set[LDiEdge[String]] = labelGraph.edges.map(PrevStep.previousStepEdgeToPrevStepEdge(labelGraph)).flatten.to[Set]
    (foundEdges -- expectedEdges) should be (Set.empty)
    foundEdges should be (expectedEdges)

    val expectedBetweenness:Map[String,Double] = Map(E -> 13.0, F -> 0.0, A -> 0.0, G -> 0.0, B -> 6.5
      , C -> 13.0, H -> 1.5, D -> 13.0)
    val foundBetweenness = labelGraphAndBetweenness._2
    foundBetweenness should be (expectedBetweenness)
  }

  def timeBrandes(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val allShortestPaths = new AllShortestPathsPredecessors[Int]

    val startTime = System.currentTimeMillis()
    val labelGraph = Brandes.shortestPathsAndBetweenness(graph)(allShortestPaths,new AllShortestPathsPredecessorsGraphBuilder[Int])
    val time = System.currentTimeMillis() - startTime

    val calibrateBigO = Math.pow(calibrate._1,2) * Math.log(calibrate._1)
    val constant = calibrate._2 / calibrateBigO

    val bigO =  Math.pow(nodeCount,2) * Math.log(nodeCount)

    val expected:Long = (constant * bigO).toLong

    println("nodeCount:"+nodeCount+" actual:"+time+" expected:"+expected)
    (nodeCount,time,expected)
  }

  /*
  "The Brandes algorithm" should "scale up at  O(|V|^2 ln|V|)" in {

    //warm up
    timeBrandes(32,(1,1,1))

    val calibrate = timeBrandes(32,(1,1,1))

    val result = (5.0.to(6.0,0.5)).map(x => timeBrandes(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
*/
}
case class PrevStep[N](steps:Int,predecessors:Set[N],numShortestPaths:Int) {}

object PrevStep {

  def previousStepEdgeToPrevStepEdge(graph:Graph[String,LDiEdge])(edge:graph.EdgeT):Option[LDiEdge[String]] = {

    val prevStep = edgeToPrevStep(graph)(edge)
    prevStep match {
      case Some(x) => Some((edge._1.value ~+> edge._2.value)(prevStep))
      case None => None
    }
  }

  def edgeToPrevStep(graph:Graph[String,LDiEdge])(edge:graph.EdgeT):Option[PrevStep[String]] = {

    edge.toEdgeIn.label.asInstanceOf[Option[PreviousStep[String]]] match {
      case Some(pStep) => {
        val previousStep:PreviousStep[String] = pStep
        Some(PrevStep(previousStep.steps,previousStep.predecessors,previousStep.numShortestPaths))
      }
      case None => None
    }
  }
}
