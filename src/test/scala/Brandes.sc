/**
 * @author dwalend
 * @since v1
 */

import scalax.collection.edge.LBase.LEdgeImplicits
import net.walend.scalagraph.minimizer.semiring.SomeGraph._
import net.walend.scalagraph.minimizer.semiring.{PrevStep, PreviousStep, BrandesFewestNodes, AllShortestPathsPredecessorsGraphBuilder, FloydWarshall, Dijkstra, AllShortestPathsPredecessors}

val allShortestPathsPredecessors = new AllShortestPathsPredecessors[String]

/*
val labelGraph = Dijkstra.allPairsShortestPaths(testGraph)(allShortestPathsPredecessors,new AllShortestPathsPredecessorsGraphBuilder[String])
for(edge <- labelGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}
*/

val floydGraph = FloydWarshall.allPairsShortestPaths(testGraph)(allShortestPathsPredecessors.semiring)(new AllShortestPathsPredecessorsGraphBuilder[String])


for(edge <- floydGraph.edges) {
  println("("+edge._1+"~+#>"+edge._2+")"+"("+edge.label+"),")
}


val brandesGraphAndBetweenness = BrandesFewestNodes.shortestPathsAndBetweenness(testGraph)(allShortestPathsPredecessors,new AllShortestPathsPredecessorsGraphBuilder[String])

brandesGraphAndBetweenness._2
/*
for(edge <- brandesGraphAndBetweenness._1.edges) {

  edge.toEdgeIn.label.asInstanceOf[Option[PreviousStep[String]]] match {
    case Some(pStep) => {
      val previousStep:PreviousStep[String] = pStep
      val prevStep:Option[PrevStep[String]] = Some(PrevStep(previousStep.steps,previousStep.predecessors,previousStep.numShortestPaths))
      println("("+edge._1+"~+#>"+edge._2+")"+"("+prevStep+"),")
    }
    case None => println("("+edge._1+"~+#>"+edge._2+")"+"("+None+"),")
  }
}
*/
brandesGraphAndBetweenness._2

