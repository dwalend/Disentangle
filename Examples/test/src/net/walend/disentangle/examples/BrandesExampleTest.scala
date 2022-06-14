package net.walend.disentangle.examples

import org.scalatest.{Matchers, FlatSpec}

import net.walend.disentangle.graph.SomeGraph._
import net.walend.disentangle.graph.semiring.Brandes.BrandesSteps

/**
 *
 *
 * @author dwalend
 * @since v0.2.0
 */
class BrandesExampleTest extends FlatSpec with Matchers {

  "The Brandes example" should "produce expected results" in {

    val expectedShortestPaths = Vector(
                                      (A,A,Some(BrandesSteps(0,1,List()))),
                                      (A,B,Some(BrandesSteps(1,1,List(1)))),
                                      (B,B,Some(BrandesSteps(0,1,List()))),
                                      (C,B,Some(BrandesSteps(3,1,List(3)))),
                                      (D,B,Some(BrandesSteps(2,1,List(4)))),
                                      (E,B,Some(BrandesSteps(1,1,List(1)))),
                                      (H,B,Some(BrandesSteps(4,1,List(2)))),
                                      (A,C,Some(BrandesSteps(2,1,List(1)))),
                                      (B,C,Some(BrandesSteps(1,1,List(2)))),
                                      (C,C,Some(BrandesSteps(0,1,List()))),
                                      (D,C,Some(BrandesSteps(3,2,List(4)))),
                                      (E,C,Some(BrandesSteps(2,2,List(1, 6)))),
                                      (H,C,Some(BrandesSteps(1,1,List(2)))),
                                      (A,D,Some(BrandesSteps(3,1,List(1)))),
                                      (B,D,Some(BrandesSteps(2,1,List(2)))),
                                      (C,D,Some(BrandesSteps(1,1,List(3)))),
                                      (D,D,Some(BrandesSteps(0,1,List()))),
                                      (E,D,Some(BrandesSteps(3,2,List(1, 6)))),
                                      (H,D,Some(BrandesSteps(2,1,List(2)))),
                                      (A,E,Some(BrandesSteps(4,1,List(1)))),
                                      (B,E,Some(BrandesSteps(3,1,List(2)))),
                                      (C,E,Some(BrandesSteps(2,1,List(3)))),
                                      (D,E,Some(BrandesSteps(1,1,List(4)))),
                                      (E,E,Some(BrandesSteps(0,1,List()))),
                                      (H,E,Some(BrandesSteps(3,1,List(2)))),
                                      (A,F,Some(BrandesSteps(5,1,List(1)))),
                                      (B,F,Some(BrandesSteps(4,1,List(2)))),
                                      (C,F,Some(BrandesSteps(3,1,List(3)))),
                                      (D,F,Some(BrandesSteps(2,1,List(4)))),
                                      (E,F,Some(BrandesSteps(1,1,List(5)))),
                                      (F,F,Some(BrandesSteps(0,1,List()))),
                                      (H,F,Some(BrandesSteps(4,1,List(2)))),
                                      (A,H,Some(BrandesSteps(5,1,List(1)))),
                                      (B,H,Some(BrandesSteps(4,1,List(2)))),
                                      (C,H,Some(BrandesSteps(3,1,List(3)))),
                                      (D,H,Some(BrandesSteps(2,1,List(4)))),
                                      (E,H,Some(BrandesSteps(1,1,List(6)))),
                                      (H,H,Some(BrandesSteps(0,1,List())))
    )
    
    val expectedBetweennesses = Map(E -> 13.0, F -> 0.0, A -> 0.0, B -> 6.5, C -> 13.0, H -> 1.5, D -> 13.0)
    
    val shortestPathsAndBetweennesses = BrandesExample.shortestPathsAndBetweenness

    shortestPathsAndBetweennesses._1 should be (expectedShortestPaths)
    shortestPathsAndBetweennesses._2 should be (expectedBetweennesses)

    val shortestPathsAndBetweennessesFromPar =BrandesExample.shortestPathsAndBetweennessFromPar

    shortestPathsAndBetweennessesFromPar._1 should be (expectedShortestPaths)
    shortestPathsAndBetweennessesFromPar._2 should be (expectedBetweennesses)

    val labelDigraph = BrandesExample.labelDigraph

    val expectedSubgraphEdges: Set[labelDigraph.InnerEdge] = Set(
      labelDigraph.edge(H,C),
      labelDigraph.edge(E,B),
      labelDigraph.edge(C,D),
      labelDigraph.edge(E,H),
      labelDigraph.edge(B,C)
    ).filter(_.isDefined).map(_.get)

    val subgraph = BrandesExample.subgraph

    subgraph should be(expectedSubgraphEdges)

    val expectedPaths = List(
      List(labelDigraph.innerNode(E).get, labelDigraph.innerNode(B).get, labelDigraph.innerNode(C).get, labelDigraph.innerNode(D).get),
      List(labelDigraph.innerNode(E).get, labelDigraph.innerNode(H).get, labelDigraph.innerNode(C).get, labelDigraph.innerNode(D).get)
    )

    val paths = BrandesExample.paths
    paths should be(expectedPaths)
  }

  "The BrandesImplicitsExample" should "produce expected results" in {

    val expectedBetweenesses = Map(
      E -> 7.5,
      F -> 0.0,
      A -> 0.0,
      B -> 5.666666666666667,
      C -> 2.5,
      H -> 0.6666666666666666,
      D -> 0.6666666666666666
    )
    val betweenesses = BrandesImplicitsExample.betweennessValues

    betweenesses should be(expectedBetweenesses)
  }
}
