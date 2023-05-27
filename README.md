Disentangle
===================

Disentangle is a kit for customizable graph algorithms in Scala. Disentangle's approach differs from other graph libraries by using Scala's existing collections and tuples wherever practical. Disentangle provides only the minimal traits and classes, and does not force you to use them. For example, Dijkstra's allPairsShortestPaths() requires only a Seq of Tuple3 edges, and returns a Seq of minimal paths between nodes. A trait and class hierarchy for Graphs exist, but is not invasive and requires no type-system yoga on your part to use. Further, I only add new parts to that hierarchy when they have some pragmatic value.

Most graph libraries available on the internet provide some way to find shortest paths, almost always via Dijkstra's algorithm. However, when you try to use the algorithm provided it doesn't match your needs and is sealed up in the black box of compiled code, custom data structures, and optimistic assumptions. Disentangle's semiring-based graph minimization algorithms let you define exactly what you want to minimize. The library's core is based on ideas presented in Cormen’s massive _Algorithms_, “A general framework for solving path problems in directed graphs,” 26.4 in my 1989 edition. The high-level semiring structures are composable, which allows for a great deal of customization.

Further, the library provides support for computational stability. The same input will reliably result in the same output. Small changes in input typically result in small changes in output. The semiring-based algorithms offer an optional nodeOrder argument to provide that stability.

## Getting Disentangle

The easiest way to include this project in yours is to add the jar files from sonatype's mvn repository.

    libraryDependencies += "net.walend.disentangle" %%% "Disentangle-Graph" % "0.3.0"


### The Latest Snapshot (When Available)

To get the latest snapshot in your build.sbt, add

    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

    libraryDependencies += "net.walend.disentangle" %% "graph" % "0.4.0-SNAPSHOT"

### Clone the Code

If you want to change Disentangle to meet your every whim, share your changes by sending me pull requests, or just mess around, clone the git repo and have at it.

    git clone https://github.com/dwalend/Disentangle.git  
    cd Disentangle
    # bend Disentangle to your will
    ./millw _.test
    cp out/??? /your/project/lib

## Algorithms

Disentangle supplies

* A [Fibonacci heap](https://github.com/dwalend/Disentangle/blob/master.2/graph/src/main/scala/net/walend/disentangle/heap/FibonacciHeap.scala) -- a generic heap that supports an efficient changeKey operation.
* The [Floyd-Warshall algorithm ](https://github.com/dwalend/Disentangle/blob/master.2/graph/src/main/scala/net/walend/disentangle/graph/semiring/FloydWarshall.scala)
* [Dijkstra's algorithm](https://github.com/dwalend/Disentangle/blob/master.2/graph/src/main/scala/net/walend/disentangle/graph/semiring/Dijkstra.scala) with a Fibonacci Heap
* [Brandes' algorithm](https://github.com/dwalend/Disentangle/blob/master.2/graph/src/main/scala/net/walend/disentangle/graph/semiring/Brandes.scala) for betweenness and all shortest paths
* [implicit pimps for Graphs](https://github.com/dwalend/Disentangle/blob/master.2/graph/src/main/scala/net/walend/disentangle/graph/semiring/package.scala) to access these methods.

### Parallel Algorithms

Disentangle supplies

* A parallel version of [Dijkstra's algorithm](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/Dijkstra.scala#L129-148) 
* A parallel version of [Brandes' algorithm ](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/Brandes.scala#L142-171)

### Performance

I've used a profiler to quench hotspots where I could find ways to speed up algorithms. I've [measured performance](http://dwalend.github.io/blog/2015/11/10/Easy-Parallel/) on graphs with up to 16384 nodes on an ec2 r3.8xlarge. (Don't start a machoflops digression. I just wanted to check the algorithms' time complexity.)

## Using Disentangle

See the [scaladoc](http://dwalend.github.io/Disentangle/v0.3.0/#net.walend.disentangle.graph.package) and [examples](https://github.com/dwalend/Disentangle/tree/master/examples/src/main/scala/net/walend/disentangle/examples).

### Using implicit methods on net.walend.graph._ graphs with [Pimped Algorithms](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/BrandesImplicitsExample.scala)

If you are already using net.walend.graph._ graphs 

    import net.walend.disentangle.graph.semiring.LabelUndigraphSemiringAlgorithms
    //or
    import net.walend.disentangle.graph.semiring.LabelDigraphSemiringAlgorithms

    ...
    
    val graph = AdjacencyLabelUndigraph(edges,nodeOrder)
    
    //call the implicit methods
    val brandesResults = graph.allLeastPathsAndBetweenness()
    
    val nextStepsAndCosts: IndexedSeq[(String, String, Option[BrandesSteps[String, Int]])] = brandesResults._1
    
    val betweennessValues: Map[String, Double] = brandesResults._2



### Finding Shortest Paths with ([Dijkstra's](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/DijkstraExample.scala) and [Floyd-Warshall](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/FloydWarshallExample.scala) Algorithms)

You'll need to bring a GenTraversable[(Node,Node,Edge)] for your graph.

```tut
    val edges = Seq(
                    ("A","B","ab"),
                    ("B","C","bc"),
                    ("C","D","cd"),
                    ("D","E","de"),
                    ("E","F","ef"),
                    ("E","B","eb"),
                    ("E","H","eh"),
                    ("H","C","hc")
                   )

    /**
     * Generate all the shortest paths in the graph
     */
    val simpleShortPathLabels = Dijkstra.allPairsShortestPaths(edges)
```
Or
```tut
    /**
     * Generate all the shortest paths in the graph in parallel
     */
    val simpleShortPathLabelsFromPar = Dijkstra.parAllPairsShortestPaths(edges)
```


### Finding Betweenness and Shortest Paths with [Brandes'](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/BrandesExample.scala) Algorithm 

    /**
     * The labels from Brandes use node indexes from a directed graph,
     * so it's best to control those via the optional nodeOrder parameter
     */
    val nodeOrder = Array("A","B","C","D","E","F","H")

    /**
     * Find shortest paths and betweenness for the graph
     */
    val shortestPathsAndBetweenness = Brandes.allLeastPathsAndBetweenness(edges,nodeOrder)

Or

    /**
     * Find shortest paths and betweenness for the graph in parallel
     */
    val shortestPathsAndBetweennessFromPar = Brandes.parAllLeastPathsAndBetweenness(edges,nodeOrder)

### [Changing the Semiring](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/DijkstraLeastWeightsExample.scala)

The methods above use a semiring that finds paths with the fewest number of nodes. You can swap out the semiring or supply your own to find least-weight paths, most-probable paths, or whatever meets your needs. 

Select or create the semiring support object to use:

    /**
     * A semiring support instance that uses double-valued labels to find the shortest paths.
     */
    val support = new AllPathsFirstSteps(LeastWeights)

    /**
     * Supply a function that can convert from a String to a Double to build up the initial graph
     * of edges. You'll probably have something more significant than this hack.
     */
    def stringToDouble(fromNode:String,toNode:String,edge:String):Double = edge.map(_.hashCode().toDouble).product

And create a function to convert the edges in your graph to edges that match the semiring support:

    /**
     * Build on AllPathsFirstSteps' convert method
     */
    val labelForEdge: (String, String, String) => support.Label = support.convertEdgeToLabel[String](stringToDouble)

Then generate all of the shortest paths:

    /**
     * Generate the first steps for all paths in the graph
     */
    val leastPathLabels: Seq[(String, String, support.Label)] = Dijkstra.allPairsLeastPaths(edges,support,labelForEdge)

Or generate them in parallel:

    /**
     * Generate the first steps for all paths in the graph in parallel
     */
    val leastPathLabelsFromPar: ParSeq[(String, String, support.Label)] = Dijkstra.parAllPairsLeastPaths(edges,support,labelForEdge)

### Semirings

Disentangle supplies some basic semirings and associated support classes

* [FewestNodes](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/FewestNodes.scala) which helps create paths that include the fewest nodes between start and end nodes
* [LeastWeights](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/LeastWeights.scala) which helps create paths that have the least (positive Double) weight sum between start and end nodes
* [MostProbable](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/MostProbable.scala) which helps create paths that have the most probable (Double between zero and one) path between start and end nodes
* [TransitiveClosure](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/TransitiveClosure.scala) which helps create all paths that connect start and end nodes

Semirings can be composed. Disentangle takes advantage of this by supplies some semirings that decorate a core semiring, and harvest additional details about the minimal paths and subgraphs explored.

* [OnePathFirstStep](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/OnePathFirstStep.scala) which finds one minimal path between start and end nodes by supplying the next node as an Option[FirstStep]
* [AllPathsFirstSteps](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/AllPathsFirstSteps.scala) which finds all minimal paths between start and end nodes by supplying a Set of possible next nodes within an Option[FirstSteps]. This semiring includes some helper methods to convert first steps to paths.


## Customizing Disentangle

Customize Disentangle with your own conversions, Semirings, and algorithms.

### Converting Your Graph to a Sequence of (Node,Node,ArcLabel) Tuples

FloydWarshall, Dijkstra, and Brandes each include a method that take sequences of (Node,Node,Edge) tuples. These methods require you to provide a function that translates your tuple into a label that fits your semiring's Label type specifier. The decorator semirings listed above each include helper functions that require a similar function to convert the tuple to the core semiring's Label. These functions are typically very straightforward to create. 

    convertEdgeToLabelFunc:(Node,Node,Edge)=>Label

These algorithms also allow for an optional Seq of nodes. This Seq controls the ordering of the algorithm's internal processing and output and can contain both extra nodes and any nodes that already exist in the edges. Take advantage of this Seq to improve computational stability.

FloydWarshall, Dijkstra, and Brandes each include a method that takes an IndexedDigraph implementation, mutable for FloydWarshall. If you use this method then you are responsible for creating the labelDigraph correctly. I included it primarily for computational efficiency, and for a possible future lazy evaluator for Dijkstra's method.  

    labelDigraph:IndexedDigraph[Node,Label]

### Creating A Custom Semiring and Other Support Classes

You will likely want to create your own Semirings to match the problems you are solving. That will be enough to run the Floyd-Warshall algorithm. However, Dijkstra's and Brandes' algorithms requires some extra methods for the heap. Implement SemiringSupport, which includes a Semiring, a HeapOrdering, and a function to convert from Labels to the heap's Keys. Here is an example that can find the [most probable paths](https://github.com/dwalend/Disentangle/blob/master/graph/src/main/scala/net/walend/disentangle/graph/semiring/MostProbable.scala):

    object MostProbable extends SemiringSupport[Double,Double] {
    
      def semiring = MostProbableSemiring
    
      def heapOrdering = MostProbableOrdering
    
      def heapKeyForLabel = {label:Label => label}

Sometimes it can be helpful to provide a possible convertEdgeToLabel function 

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

### New Algorithms

* More development of the clustering prototype
* JIT heap optimization of Dijkstra's algorithm
* A* 
* Lazy Dijkstra's
* MST using a Heap

### More Semirings

### Concurrent Graph Minimization

* Concurrent Graph structure
* Parallel queued graph minimization
* Parallel A* variations

## Changes in 0.3.0, the seventh release

* Build with mill
* Update to latest Scala and Scala JS - That's Scala 3.2 !

## Changes in 0.2.2, the sixth release

* Upgraded to Scala version 2.12.4

## Changes in 0.2.1, the fifth release

* Added implicit methods to graphs for algorithms via pimping in net.walend.disentangle.graph.{LabelDigraphSemiringAlgorithms, LabelUndigraphSemiringAlgorithms}
** The undirected graph versions correct Brandes betweenness in undirected graphs (by dividing by 2).
* Added Undigraph, a trait for undirected graphs
* Added hierarchy down to AdjacencyLabelUndigraph, an immutable adjacency list labeled undirected digraph.
* Added a proof-of-concept clustering algorithm. (Isolated in the graph test .jar. Definitely not ready for general use.)

## Changes in 0.2.0, the forth release

* [Renamed the project Disentangle](http://dwalend.github.io/blog/2015/11/03/Rename-to-Disentangle/) from ScalaGraphMinimizer (which was nearly impossible to say).
* Added [parallel versions](http://dwalend.github.io/blog/2015/11/10/Easy-Parallel/) of Dijkstra's and Brandes' algorithms for all shortest paths.
* Restructured into subprojects to minimize dependencies on third-party libraries in your code
** Added an [example subproject](https://github.com/dwalend/Disentangle/tree/master/examples/src/main/scala/net/walend/disentangle/examples), [benchmark subproject](https://github.com/dwalend/Disentangle/tree/master/benchmark/src/main/scala/net/walend/disentangle/graph/semiring/benchmark), and [toScalaGraph subproject](https://github.com/dwalend/Disentangle/tree/master/toScalaGraph/src/main/scala/net/walend/disentangle/scalagraph/semiring/ConvertToLabelDigraph.scala)
* Started tracking [performance](http://dwalend.github.io/blog/2015/11/10/Easy-Parallel/) (No javascript rendering in README.mds, but [try this page on your own](https://github.com/dwalend/Disentangle/blob/master/benchmark/src/main/html/plot.html).)
* Added [helper methods](https://github.com/dwalend/Disentangle/blob/master/examples/src/main/scala/net/walend/disentangle/examples/DijkstraExample.scala) to some semirings to produce shortest paths.

## License and Contributions

Disentangle carries the MIT license and is (c) David Walend 2013,2014,2015,2016

Special thanks to Peter Empen for [scala-graph](http://www.scala-graph.org/), advice, code, and patience. And thanks to Aleksandar Prokopec for some answers about the [parallel collections](http://docs.scala-lang.org/overviews/parallel-collections/overview.html).

[![Latest version](https://index.scala-lang.org/net.walend.disentangle/<repository>/<artifact>/latest.svg)](https://index.scala-lang.org/<organization>/<repository>/<artifact>)