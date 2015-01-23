package net.walend.graph.semiring

import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source
import scala.pickling._
import scala.pickling.json._

/**
 * Test some algorithms vs the Enron metadata for April 2000.
 *
 * @author dwalend
 * @since v0.1.2
 */
class EnronTest extends FlatSpec with Matchers {

  "Betweenness for Enron data" should "be calculatable" in {
    val support = FewestNodes

    val fileContents = Source.fromURL(getClass.getResource("/Enron2000Apr.json")).mkString
    val edges = JSONPickle(fileContents).unpickle[Seq[(String,String,Int)]]

    val labelGraphAndBetweenness = Brandes.allLeastPathsAndBetweenness(edges,Seq.empty,support,FewestNodes.convertEdgeToLabel)
  }

}
