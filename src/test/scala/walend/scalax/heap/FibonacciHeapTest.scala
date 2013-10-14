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

  it should "not be empty after a heap with two item has that item removed" in {
    val heap = twoMemberHeap

    heap.takeMin()
    heap.isEmpty should be(false)
  }

  it should "not change what is at the top of the heap if a key in the heap changes to be higher" in {

    val heap = emptyHeap
    val a = new DoubleHeapMember("A")
    val b = new DoubleHeapMember("B")

    heap.insert(1,a)
    heap.insert(2,b)

    heap.changeKey(3,b)

    heap.takeMin() should be(a)
  }

  it should "change what is at the top of the heap if a key in the heap changes to be lower than the min" in {

    val heap = emptyHeap
    val a = new DoubleHeapMember("A")
    val b = new DoubleHeapMember("B")

    heap.insert(1,a)
    heap.insert(2,b)

    heap.changeKey(0,b)

    heap.takeMin() should be(b)
  }

  it should "change what is at the top of the heap if the current min in the heap changes to be lower than another key" in {

    val heap = emptyHeap
    val a = new DoubleHeapMember("A")
    val b = new DoubleHeapMember("B")

    heap.insert(1,a)
    heap.insert(2,b)

    heap.changeKey(3,a)

    heap.takeMin() should be(b)
  }

  it should "be able to handle a lot of operations on a larger heap" in {
    val heap = emptyHeap
    val a = new DoubleHeapMember("A")
    val b = new DoubleHeapMember("B")
    val c = new DoubleHeapMember("C")
    val d = new DoubleHeapMember("D")
    val e = new DoubleHeapMember("E")
    val f = new DoubleHeapMember("F")
    val g = new DoubleHeapMember("G")
    val h = new DoubleHeapMember("H")

    heap.insert(8,a)
    heap.insert(7,b)
    heap.insert(6,c)
    heap.insert(5,d)
    heap.insert(4,e)
    heap.insert(3,f)
    heap.insert(2,g)
    heap.insert(1,h)

    heap.takeMin() should be(h)
    heap.changeKey(0,g)
    heap.changeKey(9,f)

    heap.remove(b)

    heap.getMin should be(g)

    heap.changeKey(10,g)

    heap.takeMin() should be(e)
    heap.takeMin() should be(d)
    heap.takeMin() should be(c)
    heap.takeMin() should be(a)
    heap.takeMin() should be(f)
    heap.takeMin() should be(g)
    heap.isEmpty should be (true)
  }

}
