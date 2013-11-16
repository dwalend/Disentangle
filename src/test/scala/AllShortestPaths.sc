/**
 *
 *
 * @author dwalend
 * @since v1
 */

import walend.scalax.semiring.SomeGraph._
import walend.scalax.semiring.{FloydWarshall, Dijkstra, AllShortestPaths, AllShortestPathsGraphBuilder}



val allShortestPaths = new AllShortestPaths[String]


val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPaths,new AllShortestPathsGraphBuilder[String])

for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}

val floydGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPaths.semiring)(new AllShortestPathsGraphBuilder[String])

for(edge <- floydGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}


