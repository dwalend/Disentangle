ScalaGraphMinimizer
===================

ScalaGraphMinimizer is a kit for customizable graph algorithms, originally built for [scala-graph](http://www.scala-graph.org/). Most graph libraries available on the internet provide some way to find shortest paths, almost always via Dijkstra's algorithm. However, when you try to use the algorithm provided it doesn't match your needs and is sealed up in the black box of compiled code, custom data structures, and incorrect assumptions. ScalaGraphMinimizer uses exposed data structures based on scala.collection Seqs and tuples. Its semiring-based graph minimization algorithms let you define exactly what you want to minimize. The library's core is based on ideas presented in Cormen’s massive _Algorithms_, “A general framework for solving path problems in directed graphs,” 26.4 in my 1989 copy. The high-level semiring structures are composable, which allows for a great deal of code reuse and customization.

The current version is 0.1.2, the forth release. I have restructured the project into subprojects, separating the core graph library from the translator to scala-graph, and creating new subprojects for benchmarks, examples, and presentations.

I am seeking feedback on just what the API should look like. Please let me know what works well and what could be
better.


## Getting ScalaGraphMinimizer

The easiest way to include this project in yours is to add the jar files from sonatype's mvn repository.

    libraryDependencies += "net.walend" %% "scalagraphminimizer" % "0.1.1"

### The Latest Snapshot (When Available)

To get the latest snapshot in your build.sbt, add

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

    libraryDependencies += "net.walend" %% "scalagraphminimizer" % "0.1.2-SNAPSHOT"

### Clone the Code

If you want to change ScalaGraphMinimizer to meet your every whim, share your changes by sending me pull requests, or just mess around, clone the git repo and have at it.

    git clone https://github.com/dwalend/ScalaGraphMinimizer.git
    cd ScalaGraphMinimizer
    sbt test package
    cp target/scala-2.11/scalagraphminimizer_2.11-0.1.2-SNAPSHOT.jar /your/projectname/lib


## Using ScalaGraphMinimizer

See the [scaladoc](http://dwalend.github.io/ScalaGraphMinimizer/v0.1.1/#net.walend.graph.package)

### Using Semiring-based algorithms (Floyd-Warshall, Dijkstra, and Brandes' Algorithms)

You'll need to

* bring a graph of your own, or at least a Seq[(Node,Node,MaybeALabel)].
* choose or create a SemiringSupport implementation, like FewestNodes.
* provide a function to convert from a (Node,Node,MaybeAnEdge) tuple to the Label defined by your SemiringSupport.
** You can use net.walend.disentangle.semiring.ConvertToLabelGraph to convert from a [scala-graph](http://www.scala-graph.org/) Graph.
* choose an algorithm to perform the minimization. You probably want to use Dijkstra's algorithm.
* arrange for your code to run the algorithm on your graph

FloydWarshall provides a Digraph[Node,Label] with your nodes and labels that contain the results of the minimization. Dijkstra provides a Seq[(Node,Node,Label)] where the labels contain the results of the minimization. Brandes provides the same Seq as Dijkstra's algorithm plus a Map[Node,Double] that holds each node's betweenness.

    import net.walend.graph.semiring.{OnePathFirstStep,FirstStep,FewestNodes,Dijkstra}
    
    //You supply the graph
    val yourEdges:Seq[(String,String,String)] = ???
    
    //find one path that traverses the fewest nodes, or None
    
    //create the support class
    val support = new OnePathFirstStep[String,Int,Int](FewestNodes)

    //this will be used to convert from the arc tuples to labels
    val labelForEdge = support.convertEdgeToLabelFunc[String](FewestNodes.convertEdgeToLabel) 

    //find the first step in a shortest path for a pair of nodes if that path exists
    val firstSteps:Seq[(String,String,Option[FirstStep[String,Int]])] = 
      Dijkstra.allPairsShortestPaths(edges = yourEdges,
                                    support = support,
                                    labelForArc = labelForArc)

    //Find the shortest paths between any pair of nodes
    val shortestPath:Option[Seq[String]] = support.leastPath(start,end)

### Algorithms

ScalaGraphMinimizer supplies

* A FibonacciHeap -- a generic heap that supports an efficient changeKey operation.
* The Floyd-Warshall algorithm
* Dijkstra's algorithm with a Fibonacci Heap
* Brandes' algorithm for betweenness

### Performance

I've used a profiler to quench hotspots where I could find ways to speed up algorithms. I've tested performance up to 2048 nodes.

#### Dijkstra

TODO fill in with svg

#### Brandes

TODO fill in

#### Floyd-Warshall

TODO fill in

### Semirings

ScalaGraphMinimizer supplies some basic semirings and associated support classes

* FewestNodes which helps create paths that include the fewest nodes between start and end nodes
* LeastWeights which helps create paths that have the least (positive Double) weight sum between start and end nodes
* MostProbable which helps create paths that have the most probable (Double between zero and one) path between start and end nodes
* TransitiveClosure which helps create all paths that connect start and end nodes

Semirings can be composed. ScalaGraphMinimizer takes advantage of this by supplies some semirings that decorate a core semiring, and harvest additional details about the minimal paths and subgraphs explored.

* OnePathFirstStep which finds one minimal path between start and end nodes by supplying the next node as an Option[FirstStep]
* AllPathsFirstSteps which finds all minimal paths between start and end nodes by supplying a Set of possible next nodes within an Option[FirstSteps]


## Customizing ScalaGraphMinimizer

Customize ScalaGraphMinimizer with your own mappings, Semirings and algorithms.

### Converting Your Graph to a Sequence of (Node,Node,ArcLabel) Tuples

FloydWarshall, Dijkstra, and Brandes each include a method that take sequences of (Node,Node,ArcLabel) tuples. These methods require you to provide a function that converts a tuple into a label that fits your semiring's Label. 

These are typically very straightforward to create. The decorator semirings listed above each include helper functions that require a similar function to convert the tuple to the core semiring's Label.

    convertEdgeToLabelFunc:(Node,Node,ArcLabel)=>Label

These methods also allow for an optional nodes Seq. This Seq can contain both extra nodes and any nodes that already exist in the edges, and can control the ordering of the algorithm's output.

FloydWarshall, Dijkstra, and Brandes each also include a method that takes an IndexedDigraph implementation, mutable for FloydWarshall. If you use this method then you are responsible for creating the labelDigraph correctly. I included it primarily for computational efficiency, and for a future lazy evaluator for Dijkstra's method.  

    labelDigraph:IndexedDigraph[Node,Label]

### Creating A Custom Semiring and Other Support Classes

You will likely want to create your own Semirings to match the problems you are solving. That will be enough to run the Floyd-Warshall algorithm. However, Dijkstra's and Brandes' algorithms requires some extra methods for the heap. Implement SemiringSupport, which includes a Semiring, a HeapOrdering, and a function to convert from Labels to the heap's Keys. Here is an example that can find the most probable paths:

    object MostProbable extends SemiringSupport[Double,Double] {
    
      def semiring = MostProbableSemiring
    
      def heapOrdering = MostProbableOrdering
    
      def heapKeyForLabel = {label:Label => label}

Sometimes it can be helpful to provide a possible convertArcToLabel function 

      def convertEdgeToLabel[Node, Label](start: Node, end: Node, label: Label): MostProbable.Label = semiring.I
    
For your Semiring supply identity and annihilator values, a method to check that a label is in the domain, and summary and extend operators. Here's an example:

      object MostProbableSemiring extends Semiring {
    
        def I = 1.0
        def O = 0.0
    
        def inDomain(label: Label): Boolean = {
          I >= label && label > O
        }
    
        def summary(fromThroughToLabel:Label,
                    currentLabel:Label):Label = {
          if(fromThroughToLabel > currentLabel) {
            fromThroughToLabel
          }
          else currentLabel
        }
    
        def extend(fromThroughLabel:Label,throughToLabel:Label):Label = {
          if ((fromThroughLabel == O) || (throughToLabel == O)) O
          else {
            fromThroughLabel * throughToLabel
          }
        }
      }
    
The HeapOrdering is actually trickier to get right than the Semiring. The Heap needs a special Key, AlwaysTop, that will always be higher than the highest possible Label and AlwaysBottom, that will only be on the bottom of the heap. The identity and annihilator sometimes work as these special values. Watch out for strange behaviors of floating point infinities and wrap-around with integers. In this example, I want a version that has the highest values on top of the heap. Note that I took a shortcut and made AlwaysTop outside of the Semiring's domain.

      /**
       * A heap ordering that puts lower numbers on the top of the heap
       */
      object MostProbableOrdering extends HeapOrdering[Double] {
    
        def lteq(x: Double, y: Double): Boolean = {
          x <= y
        }
    
        /**
         * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared
         */
        //todo look again for a version that handles NaNs and infinities
        def tryCompare(x: Double, y: Double): Option[Int] = {
          Option(x.compareTo(y))
        }
    
        def keyDomainDescription = "between one and zero (the annihilator)"
    
        /**
         * @throws IllegalArgumentException if the key is unusable
         */
        def checkKey(key: Double): Unit = {
          require((MostProbable.MostProbableSemiring.inDomain(key)||(key == MostProbable.MostProbableSemiring.O)),s"Key must be $keyDomainDescription, not $key")
        }
    
        /**
         * Minimum value for the DoubleHeap
         */
        def AlwaysTop:Double = semiring.I + 0.01
    
        /**
         * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
         */
        def AlwaysBottom: Double = semiring.O
      }
    }


## Roadmap for Future Work

### Next Big Release

* Louvain community detection
* A*
* Enron test set
* Timing study with automatically generated graphs (in the test stage) (And comparison with Jung and scala-graph's own)


### API Release and Feedback

* Get some feedback on what the API should look like.
* How should OnePath and AllPaths return a result? Should the basic API provide an Iterable or a Stream? Should AllPaths return a subgraph?

### New Algorithms

* Lazy Dijkstra's
* MST using a Heap
* A* and some variations


### More Semirings



### Concurrent Graph Minization

* Concurrent Graph structure
* Parallel queued graph minimization
* Parallel A* variations
* Parallel Louvain


## License and Contributions

ScalaGraphMinimizer carries the MIT license and is (c) David Walend 2013,2014

Special thanks to Peter Empen for [scala-graph](http://www.scala-graph.org/), advice, code, and patience.

