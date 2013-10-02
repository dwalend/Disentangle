
/**
 *
 *
 * @author dwalend
 * @since 8/16/13 4:25 PM
 */

val graph = SomeGraph.graph


FloydWarshall.allPairsShortestPaths(graph)(TransitiveClosureSemiring)(TransitiveClosureLabelGraphBuilder)

FloydWarshall.allPairsShortestPaths(graph)(CountFewestNodesBetweenSemiring)(CountFewestNodesBetweenGraphBuilder)

FloydWarshall.allPairsShortestPaths(graph)(new OneShortestPathSemiring[String])(new OneShortestPathGraphBuilder[String])

val allPairs = FloydWarshall.allPairsShortestPaths(graph)(new AllShortestPathsSemiring[String])(new AllShortestPathsGraphBuilder[String])

println(allPairs.edges.mkString("\n"))

