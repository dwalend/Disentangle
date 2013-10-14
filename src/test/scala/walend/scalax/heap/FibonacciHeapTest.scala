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

  "A newly created heap" should " be empty" in {
    val heap:FibDoubleHeap = new FibDoubleHeap()
    heap.isEmpty should be(true)
  }

  it should "throw an IllegalStateException when asked for its min value" in {
    val heap:FibDoubleHeap = new FibDoubleHeap
    a [IllegalStateException] should be thrownBy {heap.getMin}
  }

}
