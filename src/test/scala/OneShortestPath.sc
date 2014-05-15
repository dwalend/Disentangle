/**
 *
 *
 * @author dwalend
 * @since v0.0.0
 */

import net.walend.scalagraph.minimizer.semiring.SomeGraph._
import net.walend.scalagraph.minimizer.semiring.{Dijkstra, OneShortestPath,OneShortestPathGraphBuilder}



val oneShortestPath = new OneShortestPath[String]


val labelGraph = Dijkstra.allPairsShortestPaths(testGraph)(oneShortestPath,new OneShortestPathGraphBuilder[String])










for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}









































