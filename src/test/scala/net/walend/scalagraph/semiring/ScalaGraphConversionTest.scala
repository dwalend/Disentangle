package net.walend.scalagraph.semiring

import net.walend.digraph.semiring.AllPathsFirstSteps
import net.walend.digraph.semiring.Brandes
import net.walend.digraph.semiring.Dijkstra
import net.walend.digraph.semiring.FewestNodes
import net.walend.digraph.semiring.FloydWarshall
import org.scalatest.{Matchers, FlatSpec}
import net.walend.digraph.{AdjacencyLabelDigraph, LabelDigraph}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.edge.LDiEdge

/**
 * Tests algorithms with a scala-graph graph
 *
 * @author dwalend
 * @since v0.1.0
 */
class ScalaGraphConversionTest extends FlatSpec with Matchers {

  //exciting example graph

  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"

  val testNodes = Set(A,B,C,D,E,F,G,H)

  import scalax.collection.edge.Implicits._

  val ab = (A ~+> B)("ab")
  val bc = (B ~+> C)("bc")
  val cd = (C ~+> D)("cd")
  val de = (D ~+> E)("de")
  val ef = (E ~+> F)("ef")
  val eb = (E ~+> B)("eb")
  val eh = (E ~+> H)("eh")
  val hc = (H ~+> C)("hc")

  val testArcs:Set[LDiEdge[String]] = Set(ab,bc,cd,de,ef,eb,eh,hc)

  val testGraph:Graph[String,LDiEdge] = Graph.from(testNodes,testArcs)

  val expectedArcs = Set(
    (A,A,0),
    (A,B,1),
    (A,C,2),
    (A,D,3),
    (A,E,4),
    (A,F,5),
    (A,H,5),
    (B,B,0),
    (B,C,1),
    (B,D,2),
    (B,E,3),
    (B,F,4),
    (B,H,4),
    (C,B,3),
    (C,C,0),
    (C,D,1),
    (C,E,2),
    (C,F,3),
    (C,H,3),
    (D,B,2),
    (D,C,3),
    (D,D,0),
    (D,E,1),
    (D,F,2),
    (D,H,2),
    (E,B,1),
    (E,C,2),
    (E,D,3),
    (E,E,0),
    (E,F,1),
    (E,H,1),
    (F,F,0),
    (G,G,0),
    (H,B,4),
    (H,C,1),
    (H,D,2),
    (H,E,3),
    (H,F,4),
    (H,H,0)
  )

  import scala.language.higherKinds
  def labelForEdge[E[X] <: EdgeLikeIn[X]](edge:E[String]):(String,String,Int) = (edge._1,edge._2,1)

  val graphParts:(Seq[(String,String,Int)],Seq[String],Int) = ConvertToLabelDigraph.convert(testGraph,FewestNodes)(labelForEdge)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertArcToLabel)

    labelGraph.arcs.to[Set] should be (expectedArcs)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertArcToLabel)

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

    val brandesSupport = new AllPathsFirstSteps[String,Int,Int](FewestNodes)

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(graphParts._1,graphParts._2,brandesSupport,FewestNodes.convertArcToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }
}
