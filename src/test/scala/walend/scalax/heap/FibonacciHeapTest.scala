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
    heap.insert(1,"A")
    heap
  }

  def twoMemberHeap:FibDoubleHeap = {
    val heap = emptyHeap
    heap.insert(1,"A")
    heap.insert(2,"B")
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
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    heap.changeKey(3,b)

    heap.takeMin() should be(a)
  }

  it should "change what is at the top of the heap if a key in the heap changes to be lower than the min" in {

    val heap = emptyHeap
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    heap.changeKey(0,b)

    heap.takeMin() should be(b)
  }

  it should "change what is at the top of the heap if the current min in the heap changes to be lower than another key" in {

    val heap = emptyHeap
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    heap.changeKey(3,a)

    heap.takeMin() should be(b)
  }

  it should "be able to handle a lot of operations on a larger heap" in {
    val heap = emptyHeap

    val a = heap.insert(8,"A")
    val b = heap.insert(7,"B")
    val c = heap.insert(6,"C")
    val d = heap.insert(5,"D")
    val e = heap.insert(4,"E")
    val f = heap.insert(3,"F")
    val g = heap.insert(2,"G")
    val h = heap.insert(1,"H")

    heap.takeMin().value should be("H")
    heap.changeKey(0,g)
    heap.changeKey(9,f)

    heap.remove(b)

    heap.getMin.value should be("G")

    heap.changeKey(10,g)

    heap.takeMin().value should be("E")
    heap.takeMin().value should be("D")
    heap.takeMin().value should be("C")
    heap.takeMin().value should be("A")

    println(heap.getMin)

    heap.takeMin().value should be("F")
    heap.takeMin().value should be("G")
    heap.isEmpty should be (true)
  }
}
