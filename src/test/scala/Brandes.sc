/**
 * @author dwalend
 * @since v1
 */

import walend.scalax.semiring.SomeGraph._
import walend.scalax.semiring.{Brandes, AllShortestPathsPredecessorsGraphBuilder, FloydWarshall, Dijkstra, AllShortestPathsPredecessors}

val allShortestPathsPredecessors = new AllShortestPathsPredecessors[String]

/*
val labelGraph = Dijkstra.allPairsShortestPaths(graph)(allShortestPathsPredecessors,new AllShortestPathsPredecessorsGraphBuilder[String])

for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}

val floydGraph = FloydWarshall.allPairsShortestPaths(graph)(allShortestPathsPredecessors.semiring)(new AllShortestPathsPredecessorsGraphBuilder[String])

for(edge <- floydGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}
*/
val brandesGraphAndBetweenness = Brandes.shortestPathsAndBetweenness(graph)(allShortestPathsPredecessors,new AllShortestPathsPredecessorsGraphBuilder[String])
brandesGraphAndBetweenness._2





