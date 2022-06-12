package net.walend.disentangle.graph

/**
 * An example graph for eyeball testing
 *
 * @author dwalend
 * @since v0.1.0
 */

object SomeGraph {

  //exciting example graph

  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"

  val testNodes = Seq(A,B,C,D,E,F,G,H)

  val ab = (A,B,"ab")
  val bc = (B,C,"bc")
  val cd = (C,D,"cd")
  val de = (D,E,"de")
  val ef = (E,F,"ef")
  val eb = (E,B,"eb")
  val eh = (E,H,"eh")
  val hc = (H,C,"hc")

  val testEdges = Seq(ab,bc,cd,de,ef,eb,eh,hc)

  val testDigraph:IndexedLabelDigraph[String,String] = AdjacencyLabelDigraph(testEdges,testNodes,"")

  val af = (A,F,"af")
  val be = (B,E,"be")

  val brandesTestEdges = Seq(ab,bc,cd,de,ef,af,be)

  val testLabeledUndirectedEdges = testEdges.map(x => (NodePair(x._1,x._2),x._3))

  val testLabelUndigraph = AdjacencyLabelUndigraph(testLabeledUndirectedEdges,testNodes)

  val testUndirectedEdges = testLabeledUndirectedEdges.map(_._1)

  val testUndigraph = AdjacencyUndigraph(testUndirectedEdges,testNodes)
}
