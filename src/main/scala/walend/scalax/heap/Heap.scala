package walend.scalax.heap

/**
 * A structure that makes an extreme value of some objects available.
 * 
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

    /**
     * If candidateKey will move the HeapMember higher than the heap, change the key. Otherwise, no change.
     * @param candidateKey proposed new key
     */
    def raiseKey(candidateKey:K):Unit
    def isInHeap: Boolean
    def remove():Unit
  }
}

object Heap {}

trait HeapOrdering[K] extends PartialOrdering[K] {

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key:K)

  /**
   * A key that will always be at the top of the heap if present at all. Used to efficiently remove items from the heap.
   */
  def AlwaysTop:K
}

/**
 * A heap ordering that puts the least Double on top.
 */
object MinDoubleHeapOrdering extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    x <= y
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    if(x<y) Some(-1)
    else if(x==y) Some(0)
    else if(x>y) Some(1)
    else None
  }

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    if(!(key > AlwaysTop)) {
      throw new IllegalArgumentException("key is "+key+" but must be greater than "+ AlwaysTop)
    }
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Double = Double.MinValue
}

