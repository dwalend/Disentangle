package walend.scalax.semiring

/**
 * An example graph for eyeball testing
 *
 * @author dwalend
 * @since v1
 */
object SomeGraph {
  import scalax.collection.edge.Implicits._
  import scalax.collection.Graph
  import scalax.collection.edge.LDiEdge


  //boring example graph

  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"

  val nodes = List(A,B,C,D,E,F,G,H)

  val ab = (A~+>B)("ab")
  val bc = (B~+>C)("bc")
  val cd = (C~+>D)("cd")
  val de = (D~+>E)("de")
  val ef = (E~+>F)("ef")
  val eb = (E~+>B)("eb")
  val eh = (E~+>H)("eh")
  val hc = (H~+>C)("hc")

  val edges = List(ab,bc,cd,de,ef,eb,eh,hc)

  val graph:Graph[String,LDiEdge] = Graph.from(nodes,edges)

}
