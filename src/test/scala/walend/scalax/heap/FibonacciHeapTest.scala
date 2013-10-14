package walend.scalax.heap

//import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Tests of a Fibonacci Heap
 *
 * @author dwalend
 * @since 10/14/13 12:59 PM
 */
class FibonacciHeapTest extends FlatSpec with Matchers {

  def emptyHeap = new FibDoubleHeap
  def oneMemberHeap:FibDoubleHeap = {
    val heap = emptyHeap
    heap.insert(1,new DoubleHeapMember("A"))
    heap
  }

  def twoMemberHeap:FibDoubleHeap = {
    val heap = emptyHeap
    heap.insert(1,new DoubleHeapMember("A"))
    heap.insert(2,new DoubleHeapMember("B"))
    heap
  }


  "A newly created heap" should " be empty" in {
    emptyHeap.isEmpty should be(true)
  }

  it should "throw an IllegalStateException when asked for its min value" in {
    a [IllegalStateException] should be thrownBy {emptyHeap.getMin}
  }

  it should "throw an IllegalStateException when asked for its min key value" in {
    a [IllegalStateException] should be thrownBy {emptyHeap.getMinKey}
  }

  it should "not be empty after an item is added" in {
    oneMemberHeap.isEmpty should be (false)
  }

  it should "be empty after a heap with one item has that item removed" in {
    val heap = oneMemberHeap
    heap.takeMin()
    heap.isEmpty should be(true)
  }

  it should "not be empty after a heap with one item has that item removed" in {
    val heap = twoMemberHeap

    heap.takeMin()

    heap.isEmpty should be(false)
  }
}
