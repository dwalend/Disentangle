package net.walend.digraph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.SomeGraph._

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class AllPathsFirstStepsTest extends FlatSpec with Matchers {

  val expectedArcs = Set[(String,String,Option[FirstSteps[String,Int]])](
    (A,A,Some(FirstSteps(0,1,Set()))),
    (A,B,Some(FirstSteps(1,1,Set(B)))),
    (A,C,Some(FirstSteps(2,1,Set(B)))),
    (A,D,Some(FirstSteps(3,1,Set(B)))),
    (A,E,Some(FirstSteps(4,1,Set(B)))),
    (A,F,Some(FirstSteps(5,1,Set(B)))),
    (A,H,Some(FirstSteps(5,1,Set(B)))),
    (B,B,Some(FirstSteps(0,1,Set()))),
    (B,C,Some(FirstSteps(1,1,Set(C)))),
    (B,D,Some(FirstSteps(2,1,Set(C)))),
    (B,E,Some(FirstSteps(3,1,Set(C)))),
    (B,F,Some(FirstSteps(4,1,Set(C)))),
    (B,H,Some(FirstSteps(4,1,Set(C)))),
    (C,B,Some(FirstSteps(3,1,Set(D)))),
    (C,C,Some(FirstSteps(0,1,Set()))),
    (C,D,Some(FirstSteps(1,1,Set(D)))),
    (C,E,Some(FirstSteps(2,1,Set(D)))),
    (C,F,Some(FirstSteps(3,1,Set(D)))),
    (C,H,Some(FirstSteps(3,1,Set(D)))),
    (D,B,Some(FirstSteps(2,1,Set(E)))),
    (D,C,Some(FirstSteps(3,1,Set(E)))),
    (D,D,Some(FirstSteps(0,1,Set()))),
    (D,E,Some(FirstSteps(1,1,Set(E)))),
    (D,F,Some(FirstSteps(2,1,Set(E)))),
    (D,H,Some(FirstSteps(2,1,Set(E)))),
    (E,B,Some(FirstSteps(1,1,Set(B)))),
    (E,C,Some(FirstSteps(2,2,Set(B, H)))),
    (E,D,Some(FirstSteps(3,2,Set(B, H)))),
    (E,E,Some(FirstSteps(0,1,Set()))),
    (E,F,Some(FirstSteps(1,1,Set(F)))),
    (E,H,Some(FirstSteps(1,1,Set(H)))),
    (F,F,Some(FirstSteps(0,1,Set()))),
    (G,G,Some(FirstSteps(0,1,Set()))),
    (H,B,Some(FirstSteps(4,1,Set(C)))),
    (H,C,Some(FirstSteps(1,1,Set(C)))),
    (H,D,Some(FirstSteps(2,1,Set(C)))),
    (H,E,Some(FirstSteps(3,1,Set(C)))),
    (H,F,Some(FirstSteps(4,1,Set(C)))),
    (H,H,Some(FirstSteps(0,1,Set())))
  )

  val support = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(testGraph.arcs,testGraph.nodes,support,support.convertArcToLabelFunc[String](FewestNodes.convertArcToLabel))

    labelGraph.arcs.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(testGraph.arcs,testGraph.nodes,support,support.convertArcToLabelFunc[String](FewestNodes.convertArcToLabel))

    labels.size should be (expectedArcs.size)
    labels.to[Set] should be (expectedArcs)
  }

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

  "Brandes' algorithm" should "produce both the correct label graph and betweenness for Somegraph" in {

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testGraph.arcs,testGraph.nodes,support,FewestNodes.convertArcToLabel)

    (labelGraphAndBetweenness._1.to[Set] -- expectedArcs) should be (Set.empty)

    labelGraphAndBetweenness._1.to[Set] should be (expectedArcs)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
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

    val allArcs = brandesTestArcs ++ brandesTestArcs.map(arc => (arc._2,arc._1,arc._3))

    val result = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertArcToLabel)

    val betweennesses = result._2.toMap

    //if only    betweennesses should be(jungBetwennesses)
    for(node <- betweennesses.keys) {
      import scala.math.abs
      val epsilon = 0.000000000000001 * betweennesses(node)
      assert(abs(betweennesses(node) - expectedB(node)) <= epsilon,s"$node's betweenness ${betweennesses(node)} does not match jung's ${expectedB(node)}")
    }
  }



  def jungBetweenness[Node,Label](arcs:Seq[(Node,Node,Label)]):Set[(Node,Double)] = {
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
    import scala.collection.JavaConversions._

    val jb = jungGraph.getVertices.to[Set].map(node => (node,jungBetweenCalc.getVertexScore(node).toDouble))

    jb
  }
/* todo turn back on when verbal expressions supports 2.10 */
  def usStateEdges = {

    import scala.io.Source
    import scala.util.matching.Regex

    /*
AL FL
AL GA
AL MS
AL TN
     */

    val lines = Source.fromURL(getClass.getResource("/contiguous-usa.dat.txt")).getLines()
    //turn them into arcs
    def arcFromLine(line:String):Option[(String,String,Unit)] = {
      import com.github.verbalexpressions.VerbalExpression
      import VerbalExpression._

      val verbalExpression:VerbalExpression = $.startOfLine()
        .beginCapture.words().endCapture
        .whitespaces()
        .beginCapture.words().endCapture
        .endOfLine()

      val regex:Regex = verbalExpression.regexp.r

      regex.findFirstMatchIn(line) match {
        case Some(m) => Some((m.group(1),m.group(2),()))
        case None => {
          println(s"Couldn't parse $line")
          None
        }
      }
    }

    val arcs = lines.map(arcFromLine).flatten.to[Seq]

    arcs
  }

  "Brandes' algorithm" should "produce the same betweenness as Jung for the US state dataset" in {

    val arcs = usStateEdges

    //now make an arc going the other way
    val allArcs = arcs ++ arcs.map(arc => (arc._2,arc._1,arc._3))

    //find betweenness
    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertArcToLabel)
    val betweennesses:Map[String,Double] = labelGraphAndBetweenness._2.to[Set].map(bet => (bet._1,(bet._2/2))).toMap

    //find betweenness with Jung
    val jungB:Map[String,Double] = jungBetweenness(arcs).toMap

    //if only    betweennesses should be(jungBetwennesses)
    for(node <- betweennesses.keys) {
      import scala.math.abs
      val epsilon = 0.000000000000001 * betweennesses(node)
      assert(abs(betweennesses(node) - jungB(node)) <= epsilon,s"$node's betweenness ${betweennesses(node)} does not match jung's ${jungB(node)}")
    }
  }
}
