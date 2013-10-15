package walend.scalax.heap

/**
 * @author dwalend
 * @since 10/15/13 6:14 PM
 */
trait Heap[K,V] {

  def isEmpty:Boolean

  def insert(key:K,value:V):HeapMember

  def topMember:HeapMember

  def topValue:V

  def topKey:K

  def takeTop():HeapMember

  trait HeapMember {
    def key:K
    def key_(newKey:K):Unit
    def isInHeap: Boolean
    def remove():Unit
  }
}
