package net.walend.disentangle.scalagraph.semiring

import net.walend.disentangle.graph.AdjacencyLabelDigraph
import net.walend.disentangle.graph.semiring.{FloydWarshall, Brandes, FewestNodes, AllPathsFirstSteps, Dijkstra}
import org.scalatest.{Matchers, FlatSpec}

import scala.collection.GenTraversable
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
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

  val testEdges:Set[LDiEdge[String]] = Set(ab,bc,cd,de,ef,eb,eh,hc)

  val testGraph:Graph[String,LDiEdge] = Graph.from(testNodes,testEdges)

  val expectedEdges = Set(
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
  def edgeToLabel[E[X] <: EdgeLikeIn[X]](edge:E[String]):(String,String,Int) = (edge._1,edge._2,1)

  val graphParts:(Seq[(String,String,Int)],Seq[String],Int) = ConvertToLabelDigraph.convert(testGraph,FewestNodes)(edgeToLabel)

  "The Floyd-Warshall algorithm" should "produce the correct label graph for Somegraph" in {

    val labelGraph = FloydWarshall.allPairsShortestPaths(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertEdgeToLabel)

    labelGraph.edges.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertEdgeToLabel)

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
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

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertEdgeToLabel)

    labelGraphAndBetweenness._2 should be (expectedBetweenness)
  }

  "Dijkstra's algorithm" should "produce the correct all paths first step graph for Somegraph" in {

    val labels = Dijkstra.allPairsShortestPaths(graphParts._1,graphParts._2,FewestNodes,FewestNodes.convertEdgeToLabel)

    labels.size should be (expectedEdges.size)
    labels.to[Set] should be (expectedEdges)
  }

  "Dijkstra's algorithm" should "produce the correct label graph for AllPathsFirstSteps" in {
    import net.walend.disentangle.graph.semiring.LeastWeights

    val expectedWeights = Set(
      (A,A,Some(0.0)),
      (A,B,Some(195.0)),
      (A,C,Some(392.0)),
      (A,D,Some(591.0)),
      (A,E,Some(792.0)),
      (A,F,Some(995.0)),
      (A,H,Some(997.0)),
      (B,B,Some(0.0)),
      (B,C,Some(197.0)),
      (B,D,Some(396.0)),
      (B,E,Some(597.0)),
      (B,F,Some(800.0)),
      (B,H,Some(802.0)),
      (C,B,Some(599.0)),
      (C,C,Some(0.0)),
      (C,D,Some(199.0)),
      (C,E,Some(400.0)),
      (C,F,Some(603.0)),
      (C,H,Some(605.0)),
      (D,B,Some(400.0)),
      (D,C,Some(597.0)),
      (D,D,Some(0.0)),
      (D,E,Some(201.0)),
      (D,F,Some(404.0)),
      (D,H,Some(406.0)),
      (E,B,Some(199.0)),
      (E,C,Some(396.0)),
      (E,D,Some(595.0)),
      (E,E,Some(0.0)),
      (E,F,Some(203.0)),
      (E,H,Some(205.0)),
      (F,F,Some(0.0)),
      (G,G,Some(0.0)),
      (H,B,Some(802.0)),
      (H,C,Some(203.0)),
      (H,D,Some(402.0)),
      (H,E,Some(603.0)),
      (H,F,Some(806.0)),
      (H,H,Some(0.0))
    )

    val expectedPairsToShortestOuterNodes = Map(
      (A,A) -> List(List(A)),
      (A,B) -> Vector(List(A, B)),
      (A,C) -> Vector(List(A, B, C)),
      (A,D) -> Vector(List(A, B, C, D)),
      (A,E) -> Vector(List(A, B, C, D, E)),
      (A,F) -> Vector(List(A, B, C, D, E, F)),
      (A,H) -> Vector(List(A, B, C, D, E, H)),
      (B,B) -> List(List(B)),
      (B,C) -> Vector(List(B, C)),
      (B,D) -> Vector(List(B, C, D)),
      (B,E) -> Vector(List(B, C, D, E)),
      (B,F) -> Vector(List(B, C, D, E, F)),
      (B,H) -> Vector(List(B, C, D, E, H)),
      (C,B) -> Vector(List(C, D, E, B)),
      (C,C) -> List(List(C)),
      (C,D) -> Vector(List(C, D)),
      (C,E) -> Vector(List(C, D, E)),
      (C,F) -> Vector(List(C, D, E, F)),
      (C,H) -> Vector(List(C, D, E, H)),
      (D,B) -> Vector(List(D, E, B)),
      (D,C) -> Vector(List(D, E, B, C)),
      (D,D) -> List(List(D)),
      (D,E) -> Vector(List(D, E)),
      (D,F) -> Vector(List(D, E, F)),
      (D,H) -> Vector(List(D, E, H)),
      (E,B) -> Vector(List(E, B)),
      (E,C) -> Vector(List(E, B, C)),
      (E,D) -> Vector(List(E, B, C, D)),
      (E,E) -> List(List(E)),
      (E,F) -> Vector(List(E, F)),
      (E,H) -> Vector(List(E, H)),
      (F,F) -> List(List(F)),
      (G,G) -> List(List(G)),
      (H,B) -> Vector(List(H, C, D, E, B)),
      (H,C) -> Vector(List(H, C)),
      (H,D) -> Vector(List(H, C, D)),
      (H,E) -> Vector(List(H, C, D, E)),
      (H,F) -> Vector(List(H, C, D, E, F)),
      (H,H) -> List(List(H))
    )
    
    def edgeToDoubleLabel[E[X] <: EdgeLikeIn[X]](edge:E[String]):(String,String,Double) = (edge._1,edge._2,edge.label.asInstanceOf[String].getBytes.map(_.asInstanceOf[Int]).sum)

    def convertEdgeToLabel[String, Label](start: String, end: String, inLabel: Double): Double = inLabel

    val support = new AllPathsFirstSteps[String,Double,Double](LeastWeights)

    val graphParts:(Seq[(String,String,Double)],Seq[String],Double) = ConvertToLabelDigraph.convert(testGraph,LeastWeights)(edgeToDoubleLabel)

    val firstSteps:Seq[(String,String,support.Label)] = Dijkstra.allPairsShortestPaths(graphParts._1,graphParts._2,support,support.convertEdgeToLabelFunc[Double](convertEdgeToLabel))

    val weights = firstSteps.map(x => (x._1,x._2,x._3.map(_.weight)))
    weights.to[Set] should be (expectedWeights)

    val firstStepsDigraph = AdjacencyLabelDigraph(firstSteps)
    val shortestPaths = firstSteps.map(edge => ((edge._1,edge._2),support.allLeastPaths(firstStepsDigraph,edge._1,edge._2)))

    val shortestOuterPaths:GenTraversable[((String,String),Seq[Seq[String]])] = for(shortestPathsBetweenNodes <- shortestPaths) yield {
      val shortOuterPaths = for(shortPath <- shortestPathsBetweenNodes._2) yield {
        shortPath.map(node => node.value)
      }
      (shortestPathsBetweenNodes._1,shortOuterPaths)
    }
    val pairsToShortestOuter = shortestOuterPaths.seq.toMap
    pairsToShortestOuter should be (expectedPairsToShortestOuterNodes)

  }

}
