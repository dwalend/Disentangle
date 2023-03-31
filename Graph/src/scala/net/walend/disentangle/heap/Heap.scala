package net.walend.disentangle.heap

/**
 * A structure that makes an extreme key of some key,value pair available
 * 
 * @author dwalend
 * @since v0.0.0
 */
trait Heap[K,V] {

  def isEmpty:Boolean

  def insert(key:K,value:V):HeapMember

  def topMember:HeapMember

  def topValue:V

  def topKey:K

  def takeTop():HeapMember

  def takeTopValue():V

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

/**
 * The heap moves the greatest value to the top, according to some HeapOrdering.
 *
 * @tparam K the key type in this heap map.
 */
trait HeapOrdering[K] extends PartialOrdering[K] {

  /**
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key:K):Unit

  /**
   * A key that will always be at the top of the heap if present at all. Used to efficiently remove items from the heap.
   */
  def AlwaysTop:K

  /**
   * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom:K
}

/**
 * A heap ordering that puts the least Double on top.
 *
 * If you imitate this code, observe that lteq and tryCompare find the inverse of what you'd have for MaxDoubleHeapOrdering.
 */
object MinDoubleHeapOrdering extends HeapOrdering[Double] {

  def lteq(x: Double, y: Double): Boolean = {
    y <= x
  }

  /**
   * @return Some negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second, or None if they can't be compared

   */
  def tryCompare(x: Double, y: Double): Option[Int] = {
    if(x>y) Option(-1)
    else if(x==y) Option(0)
    else if(x<y) Option(1)
    else None
  }

  /**
   * Use require to check the key.
   *
   * @throws IllegalArgumentException if the key is unusable
   */
  def checkKey(key: Double): Unit = {
    require(key > AlwaysTop,"key is "+key+" but must be greater than "+ AlwaysTop)
  }

  /**
   * Minimum value for the DoubleHeap
   */
  def AlwaysTop:Double = Double.NegativeInfinity

  /**
   * A key that will among items on the bottom of the heap. Used primarily to add items that will eventually flow higher.
   */
  def AlwaysBottom: Double = Double.PositiveInfinity
}

