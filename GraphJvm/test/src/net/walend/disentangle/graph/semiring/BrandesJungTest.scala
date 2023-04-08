package net.walend.disentangle.graph.semiring

import net.walend.disentangle.graph.{AdjacencyLabelDigraph, LabelDigraph}
import Brandes.BrandesSteps
import munit.FunSuite

import scala.collection.Map

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class BrandesJungTest extends FunSuite {

  val support: FewestNodes.type = FewestNodes
  val brandesSupport: Brandes.BrandesSupport[String, Int, Int] = Brandes.BrandesSupport[String]()

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

  test("Brandes' algorithm should produce the same betweenness as Jung for the US state dataSeq, even in parallel") {

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
}
