package net.walend.graph.results

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
    val png = global.dataToPng("benchmark/results/v0.1.2/dijkstra.csv")

    println(png)
  }

}

object AlgorithmTime extends js.Any {

 def dataToPng(filename:String):String = js.native

}