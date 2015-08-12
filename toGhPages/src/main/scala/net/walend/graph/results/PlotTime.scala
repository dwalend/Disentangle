package net.walend.graph.results

import java.io.File

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/**
 * Plot xy results and write the image to a file
 *
 * @author dwalend
 * @since v0.1.2
 */

object PlotTime extends js.JSApp {

  def main(): Unit = {

    println("Hello from scala")
    global.hello()

    val png = global.dataToPng("file:///Users/dwalend/projects/ScalaGraphMinimizer/benchmark/results/v0.1.2/dijkstra.csv")

    println(png)
  }

}
