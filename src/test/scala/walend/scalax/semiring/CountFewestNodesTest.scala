package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.Graph
import MLDiEdge._

import walend.scalax.gengraph.GraphFactory
import scala.util.Random

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since v1
 */
class CountFewestNodesTest extends FlatSpec with Matchers {
  
  val _0: java.lang.Integer = 0
  val _1: java.lang.Integer = 1
  val _2: java.lang.Integer = 2
  val _3: java.lang.Integer = 3
  val _4: java.lang.Integer = 4
  val _5: java.lang.Integer = 5

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(_1),
      (A~+>A)(_0),
      (B~+>C)(_1),
      (B~+>B)(_0),
      (C~+>C)(_0),
      (C~+>D)(_1),
      (D~+>D)(_0),
      (D~+>E)(_1),
      (E~+>B)(_1),
      (E~+>F)(_1),
      (E~+>H)(_1),
      (E~+>E)(_0),
      (F~+>F)(_0),
      (G~+>G)(_0),
      (H~+>C)(_1),
      (H~+>H)(_0)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(_1),
      (A~+>A)(_0),
      (B~+>C)(_1),
      (B~+>B)(_0),
      (C~+>C)(_0),
      (C~+>D)(_1),
      (D~+>D)(_0),
      (D~+>E)(_1),
      (E~+>B)(_1),
      (E~+>F)(_1),
      (E~+>H)(_1),
      (E~+>E)(_0),
      (F~+>F)(_0),
      (G~+>G)(_0),
      (H~+>C)(_1),
      (H~+>H)(_0),
      (A~+>C)(_3)
    )

    CountFewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,3)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(_1),
      (A~+>A)(_0),
      (B~+>C)(_1),
      (B~+>B)(_0),
      (C~+>C)(_0),
      (C~+>D)(_1),
      (D~+>D)(_0),
      (D~+>E)(_1),
      (E~+>B)(_1),
      (E~+>F)(_1),
      (E~+>H)(_1),
      (E~+>E)(_0),
      (F~+>F)(_0),
      (G~+>G)(_0),
      (H~+>C)(_1),
      (H~+>H)(_0)
    )

    CountFewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,CountFewestNodesSemiring.O)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }


  "The Floyd-Warshall algorithm" should "produce a label graph where each node is reachable from itself" in {
    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesSemiring)(CountFewestNodesGraphBuilder)

    for(node <- labelGraph.nodes) {
      node ~>? node match {
        case Some(edge:labelGraph.EdgeT) => {
          assert(edge.label==CountFewestNodesSemiring.I,"The edge label for all self-edges should be "+CountFewestNodesSemiring.I+" but for "+node+" it is "+edge.label)
        }
        case Some(x) => fail("Unexpected type "+x.getClass+" for label edge "+x+", the self-edge for "+node)
        case None => fail("No self-edge for "+node)
      }
    }
  }

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesSemiring)(CountFewestNodesGraphBuilder)

    val expectedEdges = Set(
      (E~+>E)(_0),
      (G~+>G)(_0),
      (A~+>F)(_5),
      (B~+>C)(_1),
      (D~+>B)(_2),
      (B~+>D)(_2),
      (B~+>B)(_0),
      (D~+>F)(_2),
      (H~+>B)(_4),
      (B~+>H)(_4),
      (E~+>D)(_3),
      (C~+>F)(_3),
      (A~+>D)(_3),
      (C~+>B)(_3),
      (C~+>C)(_0),
      (A~+>E)(_4),
      (H~+>E)(_3),
      (E~+>F)(_1),
      (H~+>C)(_1),
      (E~+>B)(_1),
      (C~+>D)(_1),
      (H~+>H)(_0),
      (A~+>B)(_1),
      (F~+>F)(_0),
      (A~+>A)(_0),
      (H~+>F)(_4),
      (A~+>H)(_5),
      (E~+>H)(_1),
      (E~+>C)(_2),
      (C~+>E)(_2),
      (D~+>C)(_3),
      (B~+>E)(_3),
      (A~+>C)(_2),
      (B~+>F)(_4),
      (C~+>H)(_3),
      (D~+>D)(_0),
      (D~+>E)(_1),
      (H~+>D)(_2),
      (D~+>H)(_2)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(CountFewestNodes,CountFewestNodesGraphBuilder)

    val expectedEdges = Set(
      (E~+>E)(_0),
      (G~+>G)(_0),
      (A~+>F)(_5),
      (B~+>C)(_1),
      (D~+>B)(_2),
      (B~+>D)(_2),
      (B~+>B)(_0),
      (D~+>F)(_2),
      (H~+>B)(_4),
      (B~+>H)(_4),
      (E~+>D)(_3),
      (C~+>F)(_3),
      (A~+>D)(_3),
      (C~+>B)(_3),
      (C~+>C)(_0),
      (A~+>E)(_4),
      (H~+>E)(_3),
      (E~+>F)(_1),
      (H~+>C)(_1),
      (E~+>B)(_1),
      (C~+>D)(_1),
      (H~+>H)(_0),
      (A~+>B)(_1),
      (F~+>F)(_0),
      (A~+>A)(_0),
      (H~+>F)(_4),
      (A~+>H)(_5),
      (E~+>H)(_1),
      (E~+>C)(_2),
      (C~+>E)(_2),
      (D~+>C)(_3),
      (B~+>E)(_3),
      (A~+>C)(_2),
      (B~+>F)(_4),
      (C~+>H)(_3),
      (D~+>D)(_0),
      (D~+>E)(_1),
      (H~+>D)(_2),
      (D~+>H)(_2)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  def timeFloyd(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(graph)(CountFewestNodes.semiring)

    val startTime = System.currentTimeMillis()
    FloydWarshall.floydWarshall(labelGraph)(CountFewestNodes.semiring)
    val time = System.currentTimeMillis() - startTime

    val expected:Long = ((Math.pow(nodeCount.toDouble/calibrate._1,3) ) * calibrate._2).toLong
    println("nodeCount:"+nodeCount+" relax calls:"+Math.pow(nodeCount.toDouble,3)+" actual:"+time+" expected:"+expected)
    (nodeCount,time,expected)
  }
/*
  "The Floyd-Warshall algorithm" should "scale up at  O(|V|^3)" in {

    //warm up
    timeFloyd(32,(1,1,1))
    timeFloyd(32,(1,1,1))
    timeFloyd(32,(1,1,1))
    timeFloyd(32,(1,1,1))

    val calibrate = timeFloyd(32,(1,1,1))

    val result = (5.0.to(9.0,0.25)).map(x => timeFloyd(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
*/
  def timeDijkstra(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)

    val startTime = System.currentTimeMillis()
    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(CountFewestNodes,CountFewestNodesGraphBuilder)
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
    timeDijkstra(32,(1,1,1))
    timeDijkstra(32,(1,1,1))
    timeDijkstra(32,(1,1,1))

    val calibrate = timeDijkstra(32,(1,1,1))

//    val result = (5.0.to(8.0,0.5)).map(x => timeDijkstra(Math.pow(2,x).toInt,calibrate))
    val result = (5.0.to(9.0,0.25)).map(x => timeDijkstra(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
*/
  def timeFindEdge(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createFullyConnectedGraph(nodeCount)
    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(graph)(CountFewestNodes.semiring)

    val nodeList1 = Random.shuffle(labelGraph.nodes.toList)
    val nodeList2 = Random.shuffle(labelGraph.nodes.toList)

    val pairs = nodeList1.zip(nodeList2)

    val startTime = System.nanoTime()

    for(pair <- pairs) {

      pair._1 ~>? pair._2
      /*
      val replacementEdge:LDiEdge[Int] = (pair._1.value ~+> pair._2.value)(42)

      labelGraph.upsert(replacementEdge)
 */
      /*
      pair._1 ~>? pair._2 match {
        case None => {
          val replacementEdge:LDiEdge[Int] = (pair._1.value ~+> pair._2.value)(42)

          labelGraph.upsert(replacementEdge)
        }
        case _ => //leave it alone
      }
      */
    }

    val time = System.nanoTime - startTime

    val expected:Long = ((nodeCount.toDouble/calibrate._1) * calibrate._2).toLong
    println("nodeCount:"+nodeCount+" actual:"+time/1000+" expected:"+expected/1000)
    (nodeCount,time,expected)
  }


  /*
  "The ~>? operator" should "take constant time and scale up linearly with the number of calls to " in {

    //warm up
    timeFindEdge(512,(1,1,1))
    val calibrate = timeFindEdge(32,(1,1,1))

    val result = (5.0.to(10.0,0.5)).map(x => timeFindEdge(Math.pow(2,x).toInt,calibrate))
    println(result)
  }
 */
}
