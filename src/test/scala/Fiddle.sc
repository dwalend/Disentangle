import net.walend.scalagraph.minimizer.semiring.{AllShortestPathsGraphBuilder, AllShortestPathsSemiring, OneShortestPathGraphBuilder, OneShortestPathSemiring, FewestNodesGraphBuilder, FewestNodesSemiring, TransitiveClosureLabelGraphBuilder, TransitiveClosureSemiring, FloydWarshall, SomeGraph}




/**
 *
 *
 * @author dwalend
 * @since v1
 */

val graph = SomeGraph.testGraph



val transitiveClosure = FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

for(edge <- transitiveClosure.edges) {
  println("("+edge._1+"~+>"+edge._2+")(true),")
}







FloydWarshall.allPairsShortestPaths(graph)(FewestNodesSemiring)(CountFewestNodesGraphBuilder)






FloydWarshall.allPairsShortestPaths(graph)(new OneShortestPathSemiring[String])(new OneShortestPathGraphBuilder[String])











val allPairs = FloydWarshall.allPairsShortestPaths(graph)(new AllShortestPathsSemiring[String])(new AllShortestPathsGraphBuilder[String])










println(allPairs.edges.mkString("\n"))







































