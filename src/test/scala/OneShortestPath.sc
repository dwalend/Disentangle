/**
 *
 *
 * @author dwalend
 * @since 11/11/13 10:56 PM
 */

import walend.scalax.semiring.SomeGraph._
import walend.scalax.semiring.{Dijkstra, OneShortestPath,OneShortestPathGraphBuilder}



val oneShortestPath = new OneShortestPath[String]


val labelGraph = Dijkstra.allPairsShortestPaths(graph)(oneShortestPath,new OneShortestPathGraphBuilder[String])










for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}









































