ScalaGraphMinimizer
===================

ScalaGraphMinimizer is a kit for customizable graph algorithms, originally built for [scala-graph](http://www.scala-graph.org/). Most graph libraries available on the internet provide some way to find shortest paths, almost always via Dijkstra's algorithm. However, when you try to use the algorithm provided it doesn't match your needs and is sealed up in the black box of compiled code, custom data structures and incorrect assumptions. ScalaGraphMinimizer uses semiring-based graph minimization algorithms let you define exactly what you want to minimize. The library's core is based on ideas presented in Cormen’s massive _Algorithms_, “A general framework for solving path problems in directed graphs,” 26.4 in my 1989 copy. The high-level semiring structures are composable, which allows for a great deal of code reuse and customization.

The current version is 0.1.0, the second release. In the first release I got feedback suggesting major refactoring to improve performance, and a request make the algorithms independent of scala-graph. To get there, I reworked almost all of the API so I incremented the middle revision number.

I am seeking feedback on just what the API should look like. Please let me know what works well and what could be
better.


## Getting ScalaGraphMinimizer (When Available)

The easiest way to include this project in yours is to add the jar files from sonatype's mvn repository. In a few days it will be available via

    libraryDependencies += "net.walend" % "graph4scalasemirings_2.11" % "0.1.0"

### The Latest Snapshot (When Available)

To get the latest snapshot in your build.sbt, add

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

    libraryDependencies += "net.walend" % "graph4scalasemirings_2.11" % "0.1.0-SNAPSHOT"

### clone the code repository

If you want to change ScalaGraphMinimizer to meet your every whim, fix some bugs and send me pull requests, or just mess around, clone the git repo and have at it.

    git clone https://github.com/dwalend/ScalaGraphMinimizer.git
    cd ScalaGraphMinimizer
    sbt test package
    cp target/scala-2.10/graph4scalasemirings_2.11-0.1.0-SNAPSHOT.jar /your/projects/lib


## Using ScalaGraphMinimizer

You'll need to

* bring a graph of your own, or at least a Seq[(Node,Node,Arc)]. 
* choose or create a SemiringSupport implementation like FewestNodes.
* provide a function to convert from a (Node,Node,Arc) tuple to the Label defined by your SemiringSupport.
** You can use net.walend.scalagraph.semiring.ConvertToLabelGraph to convert from a [scala-graph](http://www.scala-graph.org/) Graph.
* choose an algorithm to perform the minimization. (You probably want to use Dijkstra's algorithm)
* arrange for your code to run the algorithm on your graph

You'll get a Digraph with your nodes and arcs that contain the results of the minimization.

For example, this code snippet finds zero or one shortest path in a directed graph with String nodes:

    import net.walend.digraph.semiring.{OnePathFirstStep,FirstStep,FewestNodes,Dijkstra}
    
    //You supply the graph
    val yourArcs:Seq[(String,String,String)] = ???
    
    //find one path that traverses the fewest nodes, or None
    val support = new OnePathFirstStep[String,Int,Int](FewestNodes)

    //will be used to convert from the arc tuples to labels
    val labelForArc = support.convertArcToLabelFunc[String](FewestNodes.convertArcToLabel) 

    //finds a shortest path for each pair of nodes if that path exists
    val labelTuples:Seq[(String,String,Option[FirstStep[String,Int]])] = 
      Dijkstra.allPairsShortestPaths(arcs = yourArcs,
                                    support = support,
                                    labelForArc = labelForArc)

    //todo get the actual shortest path

### Algorithms

For the second release, ScalaGraphMinimizer supplies

* The Floyd-Warshall algorithm
* Dijkstra's algorithm with a Fibonacci Heap
* Brandes' algorithm for betweenness

Peter Empen optimized scala-graph's internal representation in scala-graph to ensure that the graph algorithms scaled at their theoretical limits. I've tested with graphs with up to 1024 nodes. I've tested just the internal representation (todo size)

* FibonacciHeap is a generic heap that supports an efficient changeKey operation.


### Semirings

ScalaGraphMinimizer supplies some basic semirings and associated support classes

* FewestNodes which helps create paths that include the fewest nodes between start and end nodes
* LeastWeights which helps create paths that have the least (positive Double) weight sum between start and end nodes
* MostProbable which helps create paths that have the most probable (Double between zero and one) path between start and end nodes
* TransitiveClosure which helps create all paths that connect start and end nodes

Semirings can be composed. ScalaGraphMinimizer takes advantage of this by supplies some semirings that decorate a core semiring, and harvest additional details about the minimal paths and subgraphs explored.

* OnePathFirstStep which finds one minimal path between start and end nodes by supplying the next node as an Option[FirstStep]
* AllPathsFirstSteps which finds all minimal paths between start and end nodes by supplying a set of possible next nodes within an Option[FirstSteps]


## Customizing ScalaGraphMinimizer

Customize ScalaGraphMinimizer with your own Semirings, and algorithms.


//todo start here
### Creating a Custom LabelGraphBuilder

You are very likely to need your own LabelGraphBuilder to create a label graph from your own specialized graph. The easiest way is to extend AbstractLabelGraphBuilder and fill in

    def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E])
                                                        (edgeT:originalGraph.EdgeT):Label

AbstractLabelGraphBuilder has a semiring available. From the originalGraph, it creates a graph with a node for each original node, a self-edge with an identity (semiring.I) label, and calls initialLabelFromGraphEdge to create a lable for MLDiEdge for each original directed edge.

In a future release I hope to provide GraphBuilders that can help with the decorator semirings. That's part of the puzzle to solve with the API in the first release.

Here's an example LabelGraphBuilder that makes Double labels:

    import scala.reflect.runtime.universe.TypeTag
    class MostProbableGraphBuilder[N:TypeTag] extends AbsractLabelGraphBuilder[N,Double](MostProbableSemiring) {

      import scalax.collection.Graph
      import scalax.collection.GraphPredef.EdgeLikeIn
      import scala.language.higherKinds

      def initialLabelFromGraphEdge[E[X] <: EdgeLikeIn[X]](originalGraph: Graph[N, E])
                                                          (edgeT: originalGraph.type#EdgeT): Double = {

        val edge:E[N] = edgeT.toOuter
        require(edge.label.isInstanceOf[Double],"Edge labels must exist and be Doubles")
        val weight = edge.label.asInstanceOf[Double]
        require(weight >= 0.0,"Edge labels must be at least 0")
        require(weight <= 1.0,"Edge labels must be at most 1")

        weight
      }
    }


If AbstractLabelGraphBuilder won't do, you can implement a LabelGraphBuilder's

    def initialLabelGraph[E[X] <: EdgeLikeIn[X]](originalGraph:Graph[N,E]):MutableGraph[N,MLDiEdge]

to do whatever is needed.

### Creating A Custom Semiring and Other Support Classes

You will likely want to create your own Semirings to match the problems you are solving. That will be enough to run the Floyd-Warshall algorithm. However, Dijkstra's algorithm requires some extra help to use it's heap. Implement GraphMinimizerSupport, which includes a Semiring, a HeapOrdering, and a function to convert from Labels to the heap's Keys.

     object MostProbable extends GraphMinimizerSupport[Double,Double] {
       def semiring = MostProbableSemiring

       def heapOrdering = MostProbableHeapOrdering

       def heapKeyForLabel = {label:Double => label}

     }

Semiring is an abstract class with a Label type parameter. Supply identity and annihilator values, and summary and extend operators. Here's an example:

    object MostProbableSemiring extends Semiring[Double] {

      //identity and annihilator
      def I = 1.0
      def O = 0.0

      /**
       * Implement this method to create the core of a summary operator
       */
      def summary(fromThroughToLabel:Double,
                  currentLabel:Double):Double = {
        if(fromThroughToLabel > currentLabel) {
          fromThroughToLabel
        }
        else currentLabel
      }

      /**
       * Implement this method to create the core of an extend operator
       */
      def extend(fromThroughLabel:Double,throughToLabel:Double):Double = {
        fromThroughLabel * throughToLabel
      }
    }

The HeapOrdering is actually trickier to get right. The Heap needs a special Key, AlwaysTop, that will always be higher than the highest possible Label and AlwaysBottom, that will only be on the bottom of the heap. The identity and annihilator sometimes have these special values. Watch out for strange behaviors of floating point infinities and extreme values of integers.

    /**
     * A heap ordering that puts higher numbers on the top of the heap
     */
    object MostProbableHeapOrdering extends HeapOrdering[Double] {

      def lteq(x: Double, y: Double): Boolean = {
        y >= x
      }

      /**
       * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

       */
      def tryCompare(x: Double, y: Double): Option[Int] = {
        val diff = x-y
        if(diff>0) Some(1)
        else if (diff<0) Some(-1)
        else Some(0)
      }

      /**
       * @throws IllegalArgumentException if the key is unusable
       */
      def checkKey(key: Double): Unit = {
        require(key >= 0.0,"Key must be zero or greater, not "+key)
        require(key <= 1.0,"Key must be one or less, not "+key)
      }

      /**
       * A top value for the DoubleHeap
       */
      def AlwaysTop:Double = 1.01

      /**
       * A key that will be among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
       */
      def AlwaysBottom: Double = 0.0
    }




## Roadmap for Future Work

### Next release

* A*
* MST using a heap and a GraphBuilder
* Enron test set
* Timing study with automatically generated graphs (in the test stage) (And comparison with Jung and scala-graph's own)


### API Release and Feedback

* Get some feedback on what the API should look like.
* How should OnePath and AllPaths return a result? Should the basic API provide an Iterable or a Stream? Should AllPaths return a subgraph?

### New Algorithms

* Lazy Dijkstra's
* MST using that Heap
* A* and some variations
* Louvain community detection


### More Semirings



### Concurrent Graph Minization

* Concurrent Graph structure
* Parallel queued graph minimization
* Parallel A* variations
* Parallel Louvain


## License and Contributions

ScalaGraphMinimizer carries the MIT license and is (c) David Walend 2013,2014

Special thanks to Peter Empen for [scala-graph](http://www.scala-graph.org/), advice, code, and patience.

