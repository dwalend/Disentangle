package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.{AdjacencyLabelDigraph, LabelDigraph, SomeGraph}
import SomeGraph._
import Brandes.BrandesSteps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.Map

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class BrandesTest extends AnyFlatSpec with Matchers {

  val expectedArcs: Set[(String, String, Option[BrandesSteps[String, Int]])] = Set[(String,String,Option[BrandesSteps[String,Int]])](
    (A,A,Some(BrandesSteps(0,1,Seq()))),
    (A,B,Some(BrandesSteps(1,1,Seq(1)))),
    (A,C,Some(BrandesSteps(2,1,Seq(1)))),
    (A,D,Some(BrandesSteps(3,1,Seq(1)))),
    (A,E,Some(BrandesSteps(4,1,Seq(1)))),
    (A,F,Some(BrandesSteps(5,1,Seq(1)))),
    (A,H,Some(BrandesSteps(5,1,Seq(1)))),
    (B,B,Some(BrandesSteps(0,1,Seq()))),
    (B,C,Some(BrandesSteps(1,1,Seq(2)))),
    (B,D,Some(BrandesSteps(2,1,Seq(2)))),
    (B,E,Some(BrandesSteps(3,1,Seq(2)))),
    (B,F,Some(BrandesSteps(4,1,Seq(2)))),
    (B,H,Some(BrandesSteps(4,1,Seq(2)))),
    (C,B,Some(BrandesSteps(3,1,Seq(3)))),
    (C,C,Some(BrandesSteps(0,1,Seq()))),
    (C,D,Some(BrandesSteps(1,1,Seq(3)))),
    (C,E,Some(BrandesSteps(2,1,Seq(3)))),
    (C,F,Some(BrandesSteps(3,1,Seq(3)))),
    (C,H,Some(BrandesSteps(3,1,Seq(3)))),
    (D,B,Some(BrandesSteps(2,1,Seq(4)))),
    (D,C,Some(BrandesSteps(3,2,Seq(4)))),
    (D,D,Some(BrandesSteps(0,1,Seq()))),
    (D,E,Some(BrandesSteps(1,1,Seq(4)))),
    (D,F,Some(BrandesSteps(2,1,Seq(4)))),
    (D,H,Some(BrandesSteps(2,1,Seq(4)))),
    (E,B,Some(BrandesSteps(1,1,Seq(1)))),
    (E,C,Some(BrandesSteps(2,2,Seq(1,7)))),
    (E,D,Some(BrandesSteps(3,2,Seq(1,7)))),
    (E,E,Some(BrandesSteps(0,1,Seq()))),
    (E,F,Some(BrandesSteps(1,1,Seq(5)))),
    (E,H,Some(BrandesSteps(1,1,Seq(7)))),
    (F,F,Some(BrandesSteps(0,1,Seq()))),
    (G,G,Some(BrandesSteps(0,1,Seq()))),
    (H,B,Some(BrandesSteps(4,1,Seq(2)))),
    (H,C,Some(BrandesSteps(1,1,Seq(2)))),
    (H,D,Some(BrandesSteps(2,1,Seq(2)))),
    (H,E,Some(BrandesSteps(3,1,Seq(2)))),
    (H,F,Some(BrandesSteps(4,1,Seq(2)))),
    (H,H,Some(BrandesSteps(0,1,Seq())))
  )

  val support: FewestNodes.type = FewestNodes

  val expectedBetweenness:Map[String,Double] = Map(
    A -> 0.0,
    B -> 6.5,
    C -> 13.0,
    D -> 13.0,
    E -> 13.0,
    F -> 0.0,
    G -> 0.0,
    H -> 1.5
  )

  val brandesSupport: Brandes.BrandesSupport[String, Int, Int] = Brandes.BrandesSupport[String]()

  def expectedSubgraphEdges(labelDigraph:LabelDigraph[String,brandesSupport.Label]): Set[labelDigraph.InnerEdgeType] = Set(
    labelDigraph.edge(H,C),
    labelDigraph.edge(E,B),
    labelDigraph.edge(C,D),
    labelDigraph.edge(E,H),
    labelDigraph.edge(B,C)
  ).filter(_.isDefined).map(_.get)

  def checkBrandesResults(labelGraphAndBetweenness:(Seq[(String, String, Option[BrandesSteps[String, Int]])], Map[String, Double])):Unit = {
    (Set.from(labelGraphAndBetweenness._1) -- expectedArcs) should be (Set.empty)

    Set.from(labelGraphAndBetweenness._1) should be (expectedArcs)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)

    val labelDigraph: AdjacencyLabelDigraph[String, brandesSupport.Label] = AdjacencyLabelDigraph(edges = labelGraphAndBetweenness._1,
      nodes = Seq.from(testDigraph.nodes),
      noEdgeExistsValue = brandesSupport.semiring.O)

    labelDigraph.innerNode(H).get

    val subgraphEdges = brandesSupport.subgraphEdges(labelDigraph,"E","D")
    subgraphEdges should be (expectedSubgraphEdges(labelDigraph))

    val expectedPaths = List(
      List(labelDigraph.innerNode(E).get, labelDigraph.innerNode(B).get, labelDigraph.innerNode(C).get, labelDigraph.innerNode(D).get),
      List(labelDigraph.innerNode(E).get, labelDigraph.innerNode(H).get, labelDigraph.innerNode(C).get, labelDigraph.innerNode(D).get)
    )

    val paths = brandesSupport.allLeastPaths(labelDigraph,"E","D")

    paths should be (expectedPaths)
  }

  "Brandes' algorithm" should "produce both the correct label graph and betweenness for SomeGraph" in {

    val labelGraphAndBetweenness: (IndexedSeq[(String, String, Option[BrandesSteps[String, Int]])], Map[String, Double]) = testDigraph.allLeastPathsAndBetweenness()
    checkBrandesResults(labelGraphAndBetweenness)
  }

  "Brandes' algorithm" should "produce the correct label graph and betweenness using the implicit method on a Digraph" in {

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testDigraph.edges,Seq.from(testDigraph.nodes),support,FewestNodes.convertEdgeToLabel)
    checkBrandesResults(labelGraphAndBetweenness)
  }


  "Brandes' algorithm" should "produce the correct label graph and betweenness using the implicit method on a Digraph in parallel" in {

    val labelGraphAndBetweenness = Brandes.parAllLeastPathsAndBetweenness(testDigraph.edges,Seq.from(testDigraph.nodes),support,FewestNodes.convertEdgeToLabel)
    checkBrandesResults((Seq.from(labelGraphAndBetweenness._1),Map.from(labelGraphAndBetweenness._2)))
  }

  "Brandes' algorithm" should "produce both the correct label graph and betweenness for a figure-8 graph" in {

    val expectedB:Map[String,Double] = Map(
      A -> 1.6666666666666665,
      B -> 6.666666666666667,
      C -> 1.6666666666666665,
      D -> 1.6666666666666665,
      E -> 6.666666666666666,
      F -> 1.6666666666666665
    )

    val allArcs = brandesTestEdges ++ brandesTestEdges.map(arc => (arc._2,arc._1,arc._3))

    val result = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertEdgeToLabel)

    val betweennesses = result._2

    //if only    betweennesses should be(jungBetwennesses)
    for(node <- betweennesses.keys) {
      import scala.math.abs
      val epsilon = 0.000000000000001 * betweennesses(node)
      assert(abs(betweennesses(node) - expectedB(node)) <= epsilon,s"$node's betweenness ${betweennesses(node)} does not match jung's ${expectedB(node)}")
    }
  }
/*
  def jungBetweenness[Node,Label](arcs:Seq[(Node,Node,Label)]):Seq[(Node,Double)] = {
    import edu.uci.ics.jung.graph.UndirectedSparseGraph
    import edu.uci.ics.jung.algorithms.scoring.BetweennessCentrality

    val jungGraph = new UndirectedSparseGraph[Node,Any]()

    val nodes = (arcs.map(_._1) ++ arcs.map(_._2)).distinct
    for(node <- nodes) {
      jungGraph.addVertex(node)
    }

    var edgeCounter = 0
    for(arc <- arcs) {
      jungGraph.addEdge(edgeCounter,arc._1,arc._2)
      edgeCounter = edgeCounter + 1
    }

    val jungBetweenCalc = new BetweennessCentrality(jungGraph)

    import scala.jdk.CollectionConverters.CollectionHasAsScala
    val jb = Seq.from(jungGraph.getVertices.asScala).map(node => (node,jungBetweenCalc.getVertexScore(node).toDouble))

    jb
  }

  def usStateEdges: Seq[(String, String, Unit)] = {

    import scala.io.Source

    /*
AL FL
AL GA
AL MS
AL TN
     */

    val lines = Source.fromURL(getClass.getResource("/contiguous-usa.dat.txt")).getLines()
    //turn them into arcs
    def arcFromLine(line:String):Option[(String,String,Unit)] = {
      val splitLine: Array[String] = line.split(" ")
      Some((splitLine(0),splitLine(1),()))
    }

    val arcs = Seq.from(lines.flatMap(arcFromLine))

    arcs
  }

  "Brandes' algorithm" should "produce the same betweenness as Jung for the US state dataSeq, even in parallel" in {

    val arcs = usStateEdges

    //now make an arc going the other way
    val allArcs = arcs ++ arcs.map(arc => (arc._2,arc._1,arc._3))

    //find betweenness
    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertEdgeToLabel)
    val betweennesses:Map[String,Double] = Seq.from(labelGraphAndBetweenness._2).map(bet => (bet._1, bet._2/2)).toMap

    //find betweenness in parallel
    val parLabelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertEdgeToLabel)
    val parBetweennesses:Map[String,Double] = Seq.from(parLabelGraphAndBetweenness._2).map(bet => (bet._1, bet._2/2)).toMap

    //find betweenness with Jung
    val jungB:Map[String,Double] = jungBetweenness(arcs).toMap

    //if only    betweennesses should be(jungBetwennesses)
    for(node <- betweennesses.keys) {
      import scala.math.abs
      val epsilon = 0.000000000000001 * betweennesses(node)
      assert(abs(betweennesses(node) - jungB(node)) <= epsilon,s"$node's betweenness ${betweennesses(node)} does not match jung's ${jungB(node)}")
      assert(abs(betweennesses(node) - parBetweennesses(node)) <= epsilon,s"$node's betweenness ${betweennesses(node)} does not match parBetweennesses's ${parBetweennesses(node)}")

    }
  }
*/
/*
  "Betweenness for Enron data" should "be calculated" in {

    import scala.io.Source
    import scala.pickling._
    import scala.pickling.json._

    val support = FewestNodes

    val fileContents = Source.fromURL(getClass.getResource("/Enron2000Apr.json")).mkString
    val edges = JSONPickle(fileContents).unpickle[Seq[(String,String,Int)]]

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(edges,Seq.empty,support,FewestNodes.convertEdgeToLabel)
  }
  */
}
