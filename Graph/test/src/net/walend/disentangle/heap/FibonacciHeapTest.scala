package net.walend.disentangle.heap

import munit.FunSuite


/**
 * Tests of a Fibonacci Heap
 *
 * @author dwalend
 * @since v0.0.0
 */
class FibonacciHeapTest extends FunSuite {

  def emptyHeap = new FibonacciHeap[Double,String](MinDoubleHeapOrdering)
  def oneMemberHeap:FibonacciHeap[Double,String] = {
    val heap = emptyHeap
    heap.insert(1,"A")
    heap
  }

  def twoMemberHeap:FibonacciHeap[Double,String] = {
    val heap = emptyHeap
    heap.insert(1,"A")
    heap.insert(2,"B")
    heap
  }

  test("A newly created heap should  be empty") {
    assertEquals(emptyHeap.isEmpty, true)
  }

  test("throw an IllegalStateException when asked for its min value") {
    intercept[IllegalStateException]{emptyHeap.topMember}
  }

  test("throw an IllegalStateException when asked for its min key value") {
    intercept[IllegalStateException]{emptyHeap.topKey}
  }

  test("not be empty after an item is added") {
    assert(!oneMemberHeap.isEmpty)
  }

  test( "be empty after a heap with one item has that item removed") {
    val heap = oneMemberHeap
    heap.takeTop()
    assertEquals(heap.isEmpty, true)
  }

  test("not be empty after a heap with two item has that item removed") {
    val heap = twoMemberHeap

    heap.takeTop()
    assertEquals(heap.isEmpty, false)
  }

  test("not change what is at the top of the heap if a key in the heap changes to be higher") {

    val heap = emptyHeap
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    b.key_(3)

    assertEquals(heap.takeTop(), a)
  }

  test("change what is at the top of the heap if a key in the heap changes to be lower than the min") {

    val heap = emptyHeap
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    b.key_(0)

    assertEquals(heap.takeTop(), b)
  }

  test("change what is at the top of the heap if the current min in the heap changes to be lower than another key") {

    val heap = emptyHeap
    val a = heap.insert(1,"A")
    val b = heap.insert(2,"B")

    a.key_(3)

    assertEquals(heap.takeTop(), b)
  }

  test("be able to handle a lot of operations on a larger heap") {
    val heap = emptyHeap

    val a:heap.FibonacciHeapMember = heap.insert(8,"A")
    val b = heap.insert(7,"B")
    val c = heap.insert(6,"C")
    val d = heap.insert(5,"D")
    val e = heap.insert(4,"E")
    val f = heap.insert(3,"F")
    val g = heap.insert(2,"G")
    val h = heap.insert(1,"H")

    assertEquals(heap.takeTop().value, "H")
    g.key_(0)
    f.key_(9)

    b.remove()

    c.remove()

    assertEquals(heap.topMember.value, "G")

    g.key_(10)

    assertEquals(heap.takeTop().value, "E")
    a.key_(-1)
    assertEquals(heap.takeTop().value, "A")
    assertEquals(heap.takeTop().value, "D")

    assertEquals(heap.takeTop().value, "F")
    assertEquals(heap.takeTop().value, "G")
    assert(heap.isEmpty)
  }
}
