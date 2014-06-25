package net.walend.graph.semiring

import org.scalatest.{Matchers, FlatSpec}
import net.walend.graph.SomeGraph._
import net.walend.graph.semiring.Brandes.{BrandesSteps, BrandesSupport}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class BrandesTest extends FlatSpec with Matchers {

  val expectedArcs = Set[(String,String,Option[BrandesSteps[String,Int]])](
    (A,A,Some(BrandesSteps(0,1,Set()))),
    (A,B,Some(BrandesSteps(1,1,Set(1)))),
    (A,C,Some(BrandesSteps(2,1,Set(1)))),
    (A,D,Some(BrandesSteps(3,1,Set(1)))),
    (A,E,Some(BrandesSteps(4,1,Set(1)))),
    (A,F,Some(BrandesSteps(5,1,Set(1)))),
    (A,H,Some(BrandesSteps(5,1,Set(1)))),
    (B,B,Some(BrandesSteps(0,1,Set()))),
    (B,C,Some(BrandesSteps(1,1,Set(2)))),
    (B,D,Some(BrandesSteps(2,1,Set(2)))),
    (B,E,Some(BrandesSteps(3,1,Set(2)))),
    (B,F,Some(BrandesSteps(4,1,Set(2)))),
    (B,H,Some(BrandesSteps(4,1,Set(2)))),
    (C,B,Some(BrandesSteps(3,1,Set(3)))),
    (C,C,Some(BrandesSteps(0,1,Set()))),
    (C,D,Some(BrandesSteps(1,1,Set(3)))),
    (C,E,Some(BrandesSteps(2,1,Set(3)))),
    (C,F,Some(BrandesSteps(3,1,Set(3)))),
    (C,H,Some(BrandesSteps(3,1,Set(3)))),
    (D,B,Some(BrandesSteps(2,1,Set(4)))),
    (D,C,Some(BrandesSteps(3,2,Set(4)))),
    (D,D,Some(BrandesSteps(0,1,Set()))),
    (D,E,Some(BrandesSteps(1,1,Set(4)))),
    (D,F,Some(BrandesSteps(2,1,Set(4)))),
    (D,H,Some(BrandesSteps(2,1,Set(4)))),
    (E,B,Some(BrandesSteps(1,1,Set(1)))),
    (E,C,Some(BrandesSteps(2,2,Set(1,7)))),
    (E,D,Some(BrandesSteps(3,2,Set(1,7)))),
    (E,E,Some(BrandesSteps(0,1,Set()))),
    (E,F,Some(BrandesSteps(1,1,Set(5)))),
    (E,H,Some(BrandesSteps(1,1,Set(7)))),
    (F,F,Some(BrandesSteps(0,1,Set()))),
    (G,G,Some(BrandesSteps(0,1,Set()))),
    (H,B,Some(BrandesSteps(4,1,Set(2)))),
    (H,C,Some(BrandesSteps(1,1,Set(2)))),
    (H,D,Some(BrandesSteps(2,1,Set(2)))),
    (H,E,Some(BrandesSteps(3,1,Set(2)))),
    (H,F,Some(BrandesSteps(4,1,Set(2)))),
    (H,H,Some(BrandesSteps(0,1,Set())))
  )

  val support = new BrandesSupport[String,Int,Int](FewestNodes)

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

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(testGraph.edges,testGraph.nodes,support,FewestNodes.convertEdgeToLabel)

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

    val allArcs = brandesTestEdges ++ brandesTestEdges.map(arc => (arc._2,arc._1,arc._3))

    val result = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertEdgeToLabel)

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
    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(allArcs,Seq.empty,support,FewestNodes.convertEdgeToLabel)
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
