package walend.scalax.semiring

/**
 * An example graph for eyeball testing
 *
 * @author dwalend
 * @since v1
 */
object SomeGraph {
  import scalax.collection.Graph
  import LDiEdge._

  //boring example graph

  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"

  val testNodes = List(A,B,C,D,E,F,G,H)

  val ab = (A~+>B)(1: Integer)
  val bc = (B~+>C)(2: Integer)
  val cd = (C~+>D)(3: Integer)
  val de = (D~+>E)(4: Integer)
  val ef = (E~+>F)(5: Integer)
  val eb = (E~+>B)(6: Integer)
  val eh = (E~+>H)(7: Integer)
  val hc = (H~+>C)(8: Integer)

  val testEdges = List(ab,bc,cd,de,ef,eb,eh,hc)

  val testGraph:Graph[String,LDiEdge] = Graph.from(testNodes,testEdges)

}
