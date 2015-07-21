package net.walend.graph.semiring.benchmark

/**
 * @author dwalend
 * @since v0.0.1
 */
object DijkstraTiming extends TimingStudy {

  def createResults(maxExponent:Int):Seq[(Int,Long,Long,Double)] = {
    TimingStudy.study(maxExponent,timeDijkstra,expectedTimeDijkstra)
  }

  def timeDijkstra(nodeCount:Int):Long = {

      import net.walend.graph.DigraphFactory
      import net.walend.graph.semiring.{AllPathsFirstSteps, Dijkstra => DDijkstra, FewestNodes => FFewestNodes}

      val support = new AllPathsFirstSteps[Int,Int,Int](FFewestNodes)
  //    val support = FFewestNodes

      val graph = DigraphFactory.createRandomNormalDigraph(nodeCount,16)

      val result = TimingStudy.timeFunction{DDijkstra.allPairsShortestPaths(graph.edges,graph.nodes.to[Seq],support,support.convertEdgeToLabelFunc[Boolean](FFewestNodes.convertEdgeToLabel))}

      result._2
    }

  def expectedTimeDijkstra(calibration:(Int,Long),nodeCount:Int):Long = {

      //O(|V|^2 ln|V|)
      def bigO(nodeCount:Int):Double = {
        Math.pow(nodeCount,2) * Math.log(nodeCount)
      }

      ((bigO(nodeCount)/bigO(calibration._1))*calibration._2).toLong
    }
/*
    def expectedTimeSingleDijkstra(calibration:(Int,Long),nodeCount:Int):Long = {

      //O(|V| ln|V|)
      def bigO(nodeCount:Int):Double = {
        nodeCount * Math.log(nodeCount)
      }

      ((bigO(nodeCount)/bigO(calibration._1))*calibration._2).toLong
    }
*/
  /*
    def timeScalaGraphConvertDijkstra(nodeCount:Int):Long = {

      import scalax.collection.Graph
      import scalax.collection.GraphEdge.DiEdge

      import net.walend.graph.semiring.{AllPathsFirstSteps, Dijkstra => DDijkstra, FewestNodes => FFewestNodes}
      import net.walend.scalagraph.semiring.ConvertToLabelDigraph

      val support:AllPathsFirstSteps[Int,Int,Int] = new AllPathsFirstSteps(FFewestNodes)

      val graph:Graph[Int,DiEdge] = GraphFactory.createRandomNormalGraph(nodeCount,16)

      import scala.language.higherKinds
      def convertToLabel[E[X] <: EdgeLikeIn[X]](edge:E[Int]):(Int,Int,Option[FirstStepsTrait[Int,Int]]) = {
        (edge._1,edge._2,Some(support.FirstSteps(1,Set.empty[Int])))
      }

      val result = timeFunction{
        val labelGraphParts = ConvertToLabelDigraph.convert(graph,support)(convertToLabel)

        def labelForLabel[N,E,L](from:N,to:N,edge:E):L = edge.asInstanceOf[L]
        DDijkstra.allPairsShortestPaths(labelGraphParts._1,labelGraphParts._2,support,labelForLabel)
      }

      result._2
    }
*/

}
