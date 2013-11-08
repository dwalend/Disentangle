import walend.scalax.semiring.{AllShortestPathsGraphBuilder, AllShortestPathsSemiring, OneShortestPathGraphBuilder, OneShortestPathSemiring, CountFewestNodesGraphBuilder, CountFewestNodesSemiring, TransitiveClosureLabelGraphBuilder, TransitiveClosureSemiring, FloydWarshall, SomeGraph}




/**
 *
 *
 * @author dwalend
 * @since 8/16/13 4:25 PM
 */

val graph = SomeGraph.graph



val transitiveClosure = FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

for(edge <- transitiveClosure.edges) {
  println("("+edge._1+"~+>"+edge._2+")(true),")
}







FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesSemiring)(CountFewestNodesGraphBuilder)






FloydWarshall.allPairsShortestPaths(graph)(new OneShortestPathSemiring[String])(new OneShortestPathGraphBuilder[String])











val allPairs = FloydWarshall.allPairsShortestPaths(graph)(new AllShortestPathsSemiring[String])(new AllShortestPathsGraphBuilder[String])










println(allPairs.edges.mkString("\n"))







































