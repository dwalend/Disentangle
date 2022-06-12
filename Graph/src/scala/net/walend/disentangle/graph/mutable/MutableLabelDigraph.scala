package net.walend.disentangle.graph.mutable

import net.walend.disentangle.graph.LabelDigraph

/**
 * A graph where edges can be upserted.
 *
 * @author dwalend
 * @since v0.1.0
 */
trait MutableLabelDigraph[Node,Label] extends LabelDigraph[Node,Label] {

  /**
   * Set the edge that spans from start to end
   *
   */
  def upsertEdge(from:InnerNodeType,to:InnerNodeType,label:Label):Unit

}