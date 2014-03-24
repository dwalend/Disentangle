ScalaGraphMinimizer
===================

ScalaGraphMinimizer is a Swiss army knife for least-subgraph (think shortest-path) algorithms built on
[scala-graph](http://www.scala-graph.org/). The semiring-based graph minimization algorithms let you define exactly what
you want to minimize. The library is based on ideas presented in Cormen’s massive _Algorithms_, “A general framework for
solving path problems in directed graphs,” 26.4 in my 1989 copy.

The current version is 0.0.1-SNAPSHOT. A release version is coming soon.

I am currently soliciting feedback on just what the API should look like. Please let me know what could be
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



Available Algorithms



Using an Existing Semiring



Creating a custom LabelGraphBuilder



Creating A New Semiring



License and Contributions


ScalaGraphMinimizer carries the MIT license and is (c) David Walend 2013-

Special thanks to Peter Empen for [scala-graph](http://www.scala-graph.org/), advice, code, and patience.

