ScalaGraphMinimizer
===================

ScalaGraphMinimizer is a Swiss army knife for least-subgraph (think shortest-path) algorithms built on
[scala-graph](http://www.scala-graph.org/). The semiring-based graph minimization algorithms let you define exactly what
you want to minimize. The library is based on ideas presented in Cormen’s massive _Algorithms_, “A general framework for
solving path problems in directed graphs,” 26.4 in my 1989 copy.

The current version is 0.0.1-SNAPSHOT. A release version is coming soon.

I am currently seeking feedback on just what the API should look like. Please let me know what could be
better and what works well.

Getting ScalaGraphMinimizer

For now, get the source code, build it via sbt, hack as you like, copy it to your lib directory, and make pull requests
if you've made things better.

git clone https://github.com/dwalend/ScalaGraphMinimizer.git
cd ScalaGraphMinimizer
sbt test package
cp target/scala-2.10/graph4scalasemirings_2.10-0.0.1-SNAPSHOT.jar /your/projects/lib

todo -- get it on mvnrepo and add the appropriate libraryDependencies lines

Using ScalaGraphMinimizer

You'll need to

* bring a graph of your own, defined as a [scala-graph](http://www.scala-graph.org/) Graph.
* create or chose a LabelGraphBuilder to convert your graph into a mutable Graph with MLDiEdges and appropriate labels.
* choose or create a GraphMinimizerSupport, with a Semiring, a HeapOrdering, and a function to translate from labels to heap keys.
* choose an algorithm to perform the minimization. (You probably want to use Dijkstra's algorithm)
todo links to other parts of this page and to javadoc

You'll get a Graph with your nodes and MLDiEdges with labels that contain the results of the minimization.

For example, this code snippet finds zero or one shortest path to in pretty much any directed graph:


    import walend.scalax.semiring.{OnePath,FewestNodes,OneShortestPathGraphBuilder,Dijkstra,MLDiEdge,Step}

    //you supply a graph
    val graph = ???

    //set up to one path that uses the fewest nodes
    val support = OnePath[N,Int,Int](FewestNodes)
    //this will be used to convert from your grap to a Graph[N,MLDiEdge] with labels of type Step
    val labelGraphBuilder = new OneShortestPathGraphBuilder[N](support.semiring)
    //this finds all shortest paths using Dijkstra's algorithm
    val labelGraph:Graph[N,MLDiEdge] = Dijkstra.allPairsShortestPaths(oneShortestPath,labelGraphBuilder)(graph)

    //this finds first step to take on the shortest path from startNode to endNode
    val firstStep:Option[Step[N,Int]] = labelGraph.get(startNode) ~>? labelGraph.get(endNode)

Using Existing Semirings



Available Algorithms


Creating a custom LabelGraphBuilder



Creating A New Semiring


Roadmap for Future Work



License and Contributions

ScalaGraphMinimizer carries the MIT license and is (c) David Walend 2013,2014

Special thanks to Peter Empen for [scala-graph](http://www.scala-graph.org/), advice, code, and patience.

