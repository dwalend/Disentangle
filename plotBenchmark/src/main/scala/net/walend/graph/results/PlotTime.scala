package net.walend.graph.results

import java.io.File

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

import scala.scalajs.js.annotation.JSExport

/**
 * Plot xy results and write the image to a file.
 *
 * Running it in node.js requires: npm install d3 jsdom xmldom xmlhttprequest .
 * (jsdom is currently limited to v3, but will get better)
 *
 * @author dwalend
 * @since v0.1.2
 */
object PlotTime extends js.JSApp {

  def main(): Unit = {

    val lineSource = LineSource("benchmark/results/v0.1.2/dijkstra.csv","nodes","expected","Expected")
    val line = lineSource.loadLine
    println(line)

//    val fileContents: js.Dynamic = global.readFile("benchmark/results/v0.1.2/dijkstra.csv")

//    println(fileContents)

//    global.plotIt("benchmark/results/v0.1.2/dijkstra.csv")

//    val png = global.dataToPng("file:///Users/dwalend/projects/ScalaGraphMinimizer/benchmark/results/v0.1.2/dijkstra.csv")

//    println(png)
  }

  def fileContentsToSomething(fileContents:String) = {
    println(fileContents)
  }

}

case class Plot(lineSources:Seq[LineSource],xAxis:Axis,yAxis:Axis)

object Plot {

  def apply(fileName:String) = {

  }

}

case class LineSource(fileName:String,xColumn:String,yColumn:String,name:String) {

  import org.scalajs.core.tools.io.NodeVirtualTextFile

  def loadLine:Line = {

    val file = new NodeVirtualTextFile(fileName)

    val allLines: List[String] = file.readLines()

    val headers = allLines.head.split(",").zipWithIndex.toMap
    val xCol:Int = headers(xColumn)
    val yCol:Int = headers(yColumn)

    val points = allLines.tail.map{text => val row = text.split(",")
                                      (java.lang.Double.parseDouble(row(headers(xColumn))),
                                        java.lang.Double.parseDouble(row(headers(yColumn))))
                                    }
    Line(name,points)
  }


}

case class Axis(title:String)

case class Line(name:String,points:Seq[(Double,Double)])

