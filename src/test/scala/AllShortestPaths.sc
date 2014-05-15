/**
 *
 *
 * @author dwalend
 * @since v0.0.0
 */

import net.walend.scalagraph.minimizer.semiring.SomeGraph._
import net.walend.scalagraph.minimizer.semiring.{FloydWarshall, Dijkstra, AllShortestPaths, AllShortestPathsGraphBuilder}



val allShortestPaths = new AllShortestPaths[String]


val labelGraph = Dijkstra.allPairsShortestPaths(testGraph)(allShortestPaths,new AllShortestPathsGraphBuilder[String])

for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}

val floydGraph = FloydWarshall.allPairsShortestPaths(testGraph)(allShortestPaths.semiring)(new AllShortestPathsGraphBuilder[String])

for(edge <- floydGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}


