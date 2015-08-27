package net.walend.graph.results

import scala.scalajs.js

import goggles.d3.all._
import goggles.svg._

import org.scalajs.dom

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

    val plot = Plot.loadPlot("plotBenchmark/src/main/resources/plotThis.json")

    println(plot)

    val lines = plot.lineSources.map(x => (x.name,x.loadLine))

    println(lines)

    val window = dom.window

    val el = window.document//.querySelector("#dataviz-container")
//    val svg = svgTag

//    println(svg(1.0).render)

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
  import org.scalajs.core.tools.io.NodeVirtualTextFile
  import upickle.default._

  def loadPlot(fileName:String) = {
    val file = new NodeVirtualTextFile(fileName)
    val json = file.content
    read[Plot](json)
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

