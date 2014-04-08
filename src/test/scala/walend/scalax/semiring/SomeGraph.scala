package walend.scalax.semiring

/**
 * An example graph for eyeball testing
 *
 * @author dwalend
 * @since v1
 */
object SomeGraph {
  import scalax.collection.Graph
  import MLDiEdge._

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

  val ab = (A~+>B)(1)
  val bc = (B~+>C)(2)
  val cd = (C~+>D)(3)
  val de = (D~+>E)(4)
  val ef = (E~+>F)(5)
  val eb = (E~+>B)(6)
  val eh = (E~+>H)(7)
  val hc = (H~+>C)(8)

  val testEdges = List(ab,bc,cd,de,ef,eb,eh,hc)

  val testGraph:Graph[String,MLDiEdge] = Graph.from(testNodes,testEdges)

}

//todo use this in all tests to check edges. Edge equality doesn't include labels or keys in scala-graph
object EdgeHelp {

  def diffSets[E](given:Set[E],expected:Set[E]):(Set[E],Set[E]) = {
    ((given -- expected),(expected -- given))
  }

  def edgeToTriplet[N](edge:MLDiEdge[N]):(N,Any,N) = {
    (edge.from,edge.label,edge.to)
  }

  def checkEdgeSets[N](given:Set[MLDiEdge[N]],expected:Set[MLDiEdge[N]]):Option[String] = {
    val givenTriplets = given.map(edgeToTriplet)
    val expectedTriplets = expected.map(edgeToTriplet)

    val diffs = diffSets(givenTriplets,expectedTriplets)

    if(diffs._1.isEmpty && diffs._2.isEmpty) None
    else {
      val result1 = if(!diffs._1.isEmpty) {
        "The given set contains items not found in expected: "+diffs._1
      } else ""
      val result2 = if(!diffs._2.isEmpty) {
        "The expected set contains items not found in given: "+diffs._2
      } else ""
      Some(result1+"\n"+result2)
    }
  }
}