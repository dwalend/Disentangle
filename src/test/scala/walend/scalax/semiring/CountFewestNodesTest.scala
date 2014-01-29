package walend.scalax.semiring

import org.scalatest.{Matchers, FlatSpec}

import SomeGraph._

import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

import TransitiveClosureSemiring.ImplicitLabel._
import walend.scalax.gengraph.GraphFactory
import walend.scalax.semiring.CountFewestNodesGraphBuilder
import scala.util.Random

/**
 * Tests Transitive Closure semiring
 *
 * @author dwalend
 * @since v1
 */
class CountFewestNodesTest extends FlatSpec with Matchers {

  "Initializing the label graph" should "produce a label graph with self-edges and edges where SomeGraph has them" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(1),
      (A~+>A)(0),
      (B~+>C)(1),
      (B~+>B)(0),
      (C~+>C)(0),
      (C~+>D)(1),
      (D~+>D)(0),
      (D~+>E)(1),
      (E~+>B)(1),
      (E~+>F)(1),
      (E~+>H)(1),
      (E~+>E)(0),
      (F~+>F)(0),
      (G~+>G)(0),
      (H~+>C)(1),
      (H~+>H)(0)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Replacing a label in the initial graph" should "only change that one label" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(1),
      (A~+>A)(0),
      (B~+>C)(1),
      (B~+>B)(0),
      (C~+>C)(0),
      (C~+>D)(1),
      (D~+>D)(0),
      (D~+>E)(1),
      (E~+>B)(1),
      (E~+>F)(1),
      (E~+>H)(1),
      (E~+>E)(0),
      (F~+>F)(0),
      (G~+>G)(0),
      (H~+>C)(1),
      (H~+>H)(0),
      (A~+>C)(3)
    )

    CountFewestNodesSemiring.replaceLabel(labelGraph)(labelGraph get A,labelGraph get C,3)

    labelGraph.edges.toEdgeInSet should be (expectedEdges)

  }

  "Replacing an annihilator in the initial graph with the annihilator" should "not change anything" in {

    val labelGraph = CountFewestNodesGraphBuilder.initialLabelGraph(testGraph)(CountFewestNodesSemiring)

    val expectedEdges = Set(
      (A~+>B)(1),
      (A~+>A)(0),
      (B~+>C)(1),
      (B~+>B)(0),
      (C~+>C)(0),
      (C~+>D)(1),
      (D~+>D)(0),
      (D~+>E)(1),
      (E~+>B)(1),
      (E~+>F)(1),
      (E~+>H)(1),
      (E~+>E)(0),
      (F~+>F)(0),
      (G~+>G)(0),
      (H~+>C)(1),
      (H~+>H)(0)
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
      (E~+>E)(0),
      (G~+>G)(0),
      (A~+>F)(5),
      (B~+>C)(1),
      (D~+>B)(2),
      (B~+>D)(2),
      (B~+>B)(0),
      (D~+>F)(2),
      (H~+>B)(4),
      (B~+>H)(4),
      (E~+>D)(3),
      (C~+>F)(3),
      (A~+>D)(3),
      (C~+>B)(3),
      (C~+>C)(0),
      (A~+>E)(4),
      (H~+>E)(3),
      (E~+>F)(1),
      (H~+>C)(1),
      (E~+>B)(1),
      (C~+>D)(1),
      (H~+>H)(0),
      (A~+>B)(1),
      (F~+>F)(0),
      (A~+>A)(0),
      (H~+>F)(4),
      (A~+>H)(5),
      (E~+>H)(1),
      (E~+>C)(2),
      (C~+>E)(2),
      (D~+>C)(3),
      (B~+>E)(3),
      (A~+>C)(2),
      (B~+>F)(4),
      (C~+>H)(3),
      (D~+>D)(0),
      (D~+>E)(1),
      (H~+>D)(2),
      (D~+>H)(2)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val graph = SomeGraph.testGraph

    val labelGraph = Dijkstra.allPairsShortestPaths(graph)(CountFewestNodes,CountFewestNodesGraphBuilder)

    val expectedEdges = Set(
      (E~+>E)(0),
      (G~+>G)(0),
      (A~+>F)(5),
      (B~+>C)(1),
      (D~+>B)(2),
      (B~+>D)(2),
      (B~+>B)(0),
      (D~+>F)(2),
      (H~+>B)(4),
      (B~+>H)(4),
      (E~+>D)(3),
      (C~+>F)(3),
      (A~+>D)(3),
      (C~+>B)(3),
      (C~+>C)(0),
      (A~+>E)(4),
      (H~+>E)(3),
      (E~+>F)(1),
      (H~+>C)(1),
      (E~+>B)(1),
      (C~+>D)(1),
      (H~+>H)(0),
      (A~+>B)(1),
      (F~+>F)(0),
      (A~+>A)(0),
      (H~+>F)(4),
      (A~+>H)(5),
      (E~+>H)(1),
      (E~+>C)(2),
      (C~+>E)(2),
      (D~+>C)(3),
      (B~+>E)(3),
      (A~+>C)(2),
      (B~+>F)(4),
      (C~+>H)(3),
      (D~+>D)(0),
      (D~+>E)(1),
      (H~+>D)(2),
      (D~+>H)(2)
    )

    labelGraph.edges.toEdgeInSet should be (expectedEdges)
  }

  def timeFloyd(nodeCount:Int,calibrate:(Int,Long,Long)):(Int,Long,Long) = {
    val graph = GraphFactory.createRandomNormalGraph(nodeCount,16)
    val allShortestPaths = new AllShortestPathsPredecessors[Int]

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

    val calibrate = timeFloyd(32,(1,1,1))

    val result = (5.0.to(6.0,0.5)).map(x => timeFloyd(Math.pow(2,x).toInt,calibrate))
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



  "The ~>? operator" should "take constant time and scale up linearly with the number of calls to " in {

    //warm up
    timeFindEdge(512,(1,1,1))
    val calibrate = timeFindEdge(32,(1,1,1))

    val result = (5.0.to(10.0,0.5)).map(x => timeFindEdge(Math.pow(2,x).toInt,calibrate))
    println(result)
  }

}
