package walend.scalax

/**
 * A toolkit for graph minimization algorithms.
 *
 * @author dwalend
 * @since v1
 */
package object semiring {

  /* making gengraph.LDiEdge package-wide visible by delegation 
   */
  type LDiEdge[N] = gengraph.LDiEdge[N]
  val  LDiEdge = gengraph.LDiEdge
  val :~> = gengraph.:~>
  val + = gengraph.+
}
