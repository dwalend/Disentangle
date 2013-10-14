package walend.scalax

/**
 * Heap trait and Heap implementations
 *
 * @author dwalend
 * @since 10/8/13 5:19 PM
 */
package object heap {

}

/**
 * A data structure that keeps the least value on the top.
 *
 * @tparam K
 * @tparam V
 */
trait Heap[K,V] {

//  def merge(heap:Heap)

  def peek:(K,V)

  def take():(K,V)

  def insert(key:K,value:V)

  def changeKey(key:K,value:V) //combination of reduceKey and remove/insert

  def remove(value:V)

}

