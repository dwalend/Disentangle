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

  val ab = (A~+>B)(1)
  val bc = (B~+>C)(2)
  val cd = (C~+>D)(3)
  val de = (D~+>E)(4)
  val ef = (E~+>F)(5)
  val eb = (E~+>B)(6)
  val eh = (E~+>H)(7)
  val hc = (H~+>C)(8)

  val edges = List(ab,bc,cd,de,ef,eb,eh,hc)

  val graph:Graph[String,LDiEdge] = Graph.from(nodes,edges)

}
