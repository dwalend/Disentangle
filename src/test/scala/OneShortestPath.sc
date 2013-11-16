/**
 *
 *
 * @author dwalend
 * @since v1
 */

import walend.scalax.semiring.SomeGraph._
import walend.scalax.semiring.{Dijkstra, OneShortestPath,OneShortestPathGraphBuilder}



val oneShortestPath = new OneShortestPath[String]


val labelGraph = Dijkstra.allPairsShortestPaths(graph)(oneShortestPath,new OneShortestPathGraphBuilder[String])










for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}









































