package net.walend.graph.results

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom

/**
 * Plot xy results and write the image to a file
 *
 * @author dwalend
 * @since v0.1.2
 */

object PlotTime extends js.JSApp {

  def main(): Unit = {
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "<strong>It works!</strong>"
    dom.document.getElementById("playground").appendChild(paragraph)
  }

  /** Computes the square of an integer.
    *  This demonstrates unit testing.
    */
  def square(x: Int): Int = x*x
}