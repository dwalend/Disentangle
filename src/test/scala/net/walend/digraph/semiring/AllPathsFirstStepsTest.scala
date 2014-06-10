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

  val expectedArcs = Set(
    (A,A,Some(FirstSteps(0,Set()))),
    (A,B,Some(FirstSteps(1,Set(B)))),
    (A,C,Some(FirstSteps(2,Set(B)))),
    (A,D,Some(FirstSteps(3,Set(B)))),
    (A,E,Some(FirstSteps(4,Set(B)))),
    (A,F,Some(FirstSteps(5,Set(B)))),
    (A,H,Some(FirstSteps(5,Set(B)))),
    (B,B,Some(FirstSteps(0,Set()))),
    (B,C,Some(FirstSteps(1,Set(C)))),
    (B,D,Some(FirstSteps(2,Set(C)))),
    (B,E,Some(FirstSteps(3,Set(C)))),
    (B,F,Some(FirstSteps(4,Set(C)))),
    (B,H,Some(FirstSteps(4,Set(C)))),
    (C,B,Some(FirstSteps(3,Set(D)))),
    (C,C,Some(FirstSteps(0,Set()))),
    (C,D,Some(FirstSteps(1,Set(D)))),
    (C,E,Some(FirstSteps(2,Set(D)))),
    (C,F,Some(FirstSteps(3,Set(D)))),
    (C,H,Some(FirstSteps(3,Set(D)))),
    (D,B,Some(FirstSteps(2,Set(E)))),
    (D,C,Some(FirstSteps(3,Set(E)))),
    (D,D,Some(FirstSteps(0,Set()))),
    (D,E,Some(FirstSteps(1,Set(E)))),
    (D,F,Some(FirstSteps(2,Set(E)))),
    (D,H,Some(FirstSteps(2,Set(E)))),
    (E,B,Some(FirstSteps(1,Set(B)))),
    (E,C,Some(FirstSteps(2,Set(B, H)))),
    (E,D,Some(FirstSteps(3,Set(B, H)))),
    (E,E,Some(FirstSteps(0,Set()))),
    (E,F,Some(FirstSteps(1,Set(F)))),
    (E,H,Some(FirstSteps(1,Set(H)))),
    (F,F,Some(FirstSteps(0,Set()))),
    (G,G,Some(FirstSteps(0,Set()))),
    (H,B,Some(FirstSteps(4,Set(C)))),
    (H,C,Some(FirstSteps(1,Set(C)))),
    (H,D,Some(FirstSteps(2,Set(C)))),
    (H,E,Some(FirstSteps(3,Set(C)))),
    (H,F,Some(FirstSteps(4,Set(C)))),
    (H,H,Some(FirstSteps(0,Set())))
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

    labelGraphAndBetweenness._1.to[Set] should be (expectedArcs)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }

  "Brandes' algorithm" should "produce the same betweenness as Jung for the Enron dataset" in {

    import scala.io.Source
    import scala.util.matching.Regex

    /*
"FROST, CARMILLA"       "AA2 35"
"KILLRAVEN/JONATHAN R"  "AA2 35"
"M'SHULLA"      "AA2 35"
"24-HOUR MAN/EMMANUEL"  "AA2 35"
"OLD SKULL"     "AA2 35"
"G'RATH"        "AA2 35"
"3-D MAN/CHARLES CHAN"  "M/PRM 35"
     */

    val lines = Source.fromURL(getClass.getResource("/16320/labeled_edges.tsv")).getLines()
    //todo start here.
    //turn them into arcs
    def arcFromLine(line:String):Option[(String,String,Unit)] = {
      import com.github.verbalexpressions.VerbalExpression
      import VerbalExpression._

      val verbalExpression:VerbalExpression = $.startOfLine().andThen("\"")
        .beginCapture.anythingBut("\"").endCapture
        .andThen("\"").whitespaces().andThen("\"")
        .beginCapture.anythingBut("\"").endCapture
        .andThen("\"").endOfLine()

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
    //find betweenness
    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(arcs,Seq.empty,support,FewestNodes.convertArcToLabel)
    val betweennesses = labelGraphAndBetweenness._2.to[Set]

    //find betweenness with Jung
    import edu.uci.ics.jung.graph.DirectedSparseGraph
    import edu.uci.ics.jung.algorithms.scoring.BetweennessCentrality

    val jungGraph = new DirectedSparseGraph[String,Any]()

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

    val jungBetwennesses = jungGraph.getVertices.to[Set].map(node => (node,jungBetweenCalc.getVertexScore(node).toDouble))

    //error if they differ
    betweennesses -- jungBetwennesses should be(Set.empty)

    betweennesses should be(jungBetwennesses)
  }



}
