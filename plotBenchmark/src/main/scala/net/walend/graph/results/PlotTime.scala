package net.walend.graph.results

import scala.scalajs.js
import scalatags.JsDom.{TypedTag, svgTags, svgAttrs}

import goggles.d3.all._
import goggles.svg._

import org.scalajs.dom

import scala.scalajs.js.Dynamic.global

import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom.html.Div
import org.scalajs.dom.svg.G

/**
 * Plot xy results and write the image to a file.
 *
 * Running it in node.js requires: npm install d3 jsdom xmldom xmlhttprequest .
 * (jsdom is currently limited to v3, but will get better)
 *
 * @author dwalend
 * @since v0.1.2
 */

@JSExport
object PlotTime extends js.JSApp {

  def main():Unit = {}

  @JSExport
  def callBack(message:String):String = {

    println(message + " in the callback!")

    message + " from the callback!"
  }

  @JSExport
  def plotD3(div2: Div//,
//             dataFileName:String,
//             plotFileName:String
              ):Unit  = {

//    val plot = Plot.loadPlot(plotFileName)

//    val points: Seq[(LineSource, Line)] = plot.lineSources.map(x => (x,x.loadLine))

    val d3 = js.Dynamic.global.d3

    val margin = Map("top" -> 30, "right" -> 20, "bottom" -> 30, "left" -> 100)
    val width = 600 - (margin("left") + margin("right"))
    val height = 270 - (margin("top") + margin("bottom"))

    // Set the ranges
    val x = d3.scale.linear().range(0, width)
    val y = d3.scale.linear().range(height, 0)

    // Define the axes
    val xAxis = d3.svg.axis().scale(x)
      .orient("bottom").ticks(5)

    val yAxis = d3.svg.axis().scale(y)
      .orient("left").ticks(5)

    // Define the line
    /*
    val valueline = d3.svg.line()
      .x(function(d) { return x(d.nodes); })
      .y(function(d) { return y(d.measured); });
          */

    // Adds the svg canvas
    val svg = d3.select(div2).append("svg")

//    svg.attr("width", 600)
//    svg.attr("height", 270)
    svg.attr("width", (width + margin("left") + margin("right")))
    svg.attr("height", height + margin("top") + margin("bottom"))

    svg.append("g").attr("transform",
        "translate(" + margin("left") + "," + margin("top") + ")")
    import js.JSConverters._

    // Scale the range of the data
    // todo use the max of all numbers that may be plotted
/*

    val maxX = points.map(_._2.points.map(p => p._1).max).max
    val maxY = points.map(_._2.points.map(p => p._2).max).max
    x.domain(Seq(0,maxX).toJSArray)
    y.domain(Seq(0,maxY).toJSArray)
     */
    x.domain(Seq(0,3260000).toJSArray)
    y.domain(Seq(0,1024).toJSArray)
    /*
        svg.selectAll("dot")
          .data(data)
          .enter().append("circle")
          .attr("r", 3.5)
          .attr("fill","blue")
          .attr("cx", function(d) { return x(d.nodes); })
          .attr("cy", function(d) { return y(d.measured); });

        // Add the scatterplot of expected values
        svg.selectAll("dot")
          .data(data)
          .enter().append("circle")
          .attr("r", 3.5)
          .attr("fill","red")
          .attr("cx", function(d) { return x(d.nodes); })
          .attr("cy", function(d) { return y(d.expected); });
    */

    // Add the X Axis

    svg.append("g")
      .attr("class", "x axis")
      .attr("transform", s"translate(0,$height)")
      .call(xAxis)

    // Add the Y Axis
    svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)

  }

  @JSExport
  def greenCircle(div2: Div):Unit = {

    val d3 = js.Dynamic.global.d3

  //    val aspectRatio = 16.0/9.0
  //    val (width, height) = goggles.api.dimensions(aspectRatio)

    val svg = d3.select(div2).append("svg")

    svg.attr("width", 600)
    svg.attr("height", 300)
    val circle = svg.append("circle")
    circle.attr("cx", 300)
    circle.attr("cy", 150)
    circle.attr("r", 30)
    circle.attr("fill", "#26963c")
    circle.attr("id", "123")

  }
/*
  @JSExport
  def greenCircle2(div2: Div):Unit = {

    val d3 = js.Dynamic.global.d3

    println(s"div2 is $div2")

    //    val aspectRatio = 16.0/9.0
    //    val (width, height) = goggles.api.dimensions(aspectRatio)

    import scalatags.JsDom.svgTags
    import scalatags.JsDom.svgAttrs
    import scalatags.JsDom.all._
    import goggles.svg.Svg

    val circle = svgTags.circle(cls:="circle",
      svgAttrs.r:="30",
      svgAttrs.fill:="green",
      svgAttrs.x:="300",
      svgAttrs.y:="150",
      id:="greenCircle"
    ).render
 //todo     .bindOption(svgAttrs.x = )

    div2.appendChild(Svg(600.0/300)(circle).render)
  }
*/
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

