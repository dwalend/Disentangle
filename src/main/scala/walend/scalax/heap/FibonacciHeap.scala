package walend.scalax.heap

//todo shouldn't need to import these. They should already be in the namespace
import walend.scalax.heap.FibonacciHeap.HeapComparator

/**
 * A generic Fibonacci heap
 *
 * @author dwalend
 * @since 10/14/13 10:17 AM
 */

class FibonacciHeap[K,V](comparator:HeapComparator[K]) //todo implement heap
{

  def isEmpty:Boolean = size==0

  def insert(key:K,value:V):HeapMember = {
    comparator.checkKey(key)

    val fibNode:HeapMember = new HeapMember(value)

    reinsert(key,fibNode)
  }

  def topMember:HeapMember = {
    checkTop()
    top
  }

  def topValue:V = {
    topMember.value
  }
  
  def topKey:K = {
    checkTop()
    top.key
  }

  def takeTop():HeapMember = {
    checkTop()
    val z:HeapMember = top
    while(z.child!=null) {
      val x:HeapMember = z.child
      z.releaseChild(x)
      z.cat(x)
    }
    z.release()

    if(z==z.right) {
      top = null
    }
    else {
      top = z.right
      consolidate()
    }
    size = size -1
    z.clean()
    z
  }

  private def changeKey(key:K,fibNode:HeapMember):Unit = {
    comparator.checkKey(key)
    comparator.tryCompare(key,fibNode.key) match {
      case Some(x) if x < 0 => raiseKey(key,fibNode)
      case Some(x) if x == 0 => // the key hasn't changed
      case Some(x) if x > 0 => {
        remove(fibNode)
        reinsert(key,fibNode)
      }
      case None => throw new IllegalArgumentException("Can not compare "+fibNode.key+" and "+key)
    }
  }

  private def remove(fibNode:HeapMember):Unit = {
    raiseKey(comparator.AlwaysTop,fibNode)
    takeTop()
  }

  //todo union
  //todo contains(HeapMember)
  //todo containsValue(Value)
  //todo containsKey(Key)
  //todo unapply to a Seq of HeapMembers
  //todo values
  //todo keys

  override def toString: String = {
    val builder = new StringBuilder()

    builder.append(this.getClass.getSimpleName+" size: "+size+"\n")
    builder.append("top: ")
    if(top==null) builder.append("null")
    else builder.append(top.toString())
    builder.append("\n")

    val it:ChildIterator = iterator()
    while(it.hasNext) {
      builder.append(it.next().toString)
      builder.append("\n")
    }
    builder.toString()
  }

  private var top:HeapMember = null
  private var size:Int = 0

  private def reinsert(key:K,fibNode:HeapMember):HeapMember = {
    comparator.checkKey(key)

    fibNode.setKeyAndInHeap(key)

    if(top != null) {
      top.cat(fibNode)
    }
    if((top==null)||(comparator.lt(fibNode.key,top.key))) {
      top = fibNode
    }
    size = size +1

    fibNode
  }

  private def cascadingCut(y: HeapMember): Unit = {
    val z: HeapMember = y.parent
    if (z != null) {
      if (!y.lostChild) {
        y.lostChild = true
      }
      else {
        cut(y, z)
        cascadingCut(z)
      }
    }
  }

  private def consolidate():Unit = {

    import scala.collection.mutable.ArraySeq
    val fibNodes:ArraySeq[HeapMember] = new ArraySeq[HeapMember](size)

    var rootCount:Int = 0
    var x:HeapMember = top
    if(x!=null) {
      do {
        rootCount = rootCount + 1
        x = x.right
      } while(x!=top)
  
      while(rootCount>0) {
        var d:Int = x.childCount

        val next:HeapMember = x.right
        while(fibNodes(d) != null) {
          var y:HeapMember = fibNodes(d)
          if(comparator.gt(x.key,y.key)) {
            val temp:HeapMember = y
            y = x
            x = temp
          }
          link(y,x)
          fibNodes(d) = null
          d = d+1
        }
        rootCount = rootCount -1
        fibNodes(d) = x
        x = next
        //and this be replaced by x.getRight(); here instead?
      }//while
      top = null
  
      for (i <- 0 until fibNodes.length) {
        if(fibNodes(i)!=null) {
          if(top!=null) {
            fibNodes(i).release()
            top.cat(fibNodes(i))
            if(comparator.lt(fibNodes(i).key,top.key)) {
              top = fibNodes(i)
            }
          }
          else {
            top = fibNodes(i) 
          }
        }//if fibNodes[i]!=null
      }//for
    }//if top!=null
  }

  /**
  Removes x from the child list of y.
    */
  private def cut(x:HeapMember,y:HeapMember):Unit = {
    y.releaseChild(x)
    top.cat(x)
  }

  /**
  Make y a child of x.
    */
  private def link(y:HeapMember,x:HeapMember):Unit = {
    //remove y from the list of the heap
    y.release()
    
    //make y a child of x
    x.addChild(y)
  }

  private def checkTop():Unit = {
    if(top==null) {
      throw new IllegalStateException("The heap is empty.")
    }
  }

  private def raiseKey(key:K,fibNode:HeapMember):Unit = {
    fibNode.setKeyAndInHeap(key)
    val y:HeapMember = fibNode.parent
    if((y!=null) && comparator.lt(fibNode.key,y.key)) {
          cut(fibNode,y)
          cascadingCut(y)
    }
    if(comparator.lt(fibNode.key,top.key)) top = fibNode
  }

  private class ChildIterator(startNode:HeapMember) {
  
    private var currentNode:HeapMember = null
    private var currentChildIterator:ChildIterator = null
    
    def hasNext:Boolean = {
      if((currentChildIterator!=null)&&(currentChildIterator.hasNext)) {
        return true
      }
      startNode != currentNode
    }
    
    def next():HeapMember = {
      if((currentChildIterator!=null)&&(currentChildIterator.hasNext)) {
        currentChildIterator.next()
      }
      else if(currentNode!=null) {
        val oldCurrentNode = currentNode
        currentNode = currentNode.right
        if(currentNode.child!=null){
          currentChildIterator = new ChildIterator(currentNode.child)
        }
        oldCurrentNode
      }
      else
      {
        currentNode = startNode.right
        if(startNode.child!=null) {
          currentChildIterator = new ChildIterator(startNode.child)
        }
        startNode
      }
    }
    
    def remove():Unit = throw new UnsupportedOperationException
  }

  private def iterator():ChildIterator = {
    new ChildIterator(top)
  }

  class HeapMember(val value:V) {

    private var _key:K = comparator.AlwaysTop
    private[FibonacciHeap] var parent: HeapMember = null //todo only need accessor outside of member
    private[FibonacciHeap] var child: HeapMember = null //todo only need accessor outside of member
    private var left: HeapMember = this
    private[FibonacciHeap] var right: HeapMember = this //todo only need accessor outside of member
    private[FibonacciHeap] var childCount: Int = 0
    private[FibonacciHeap] var lostChild: Boolean = false
    private var inHeap: Boolean = false

    private def toDebugString: String = {
      val builder: StringBuffer = new StringBuffer
      builder.append("key: " + _key + "value: "+value)
      builder.append(" lostChild: " + lostChild)
      builder.append(" left: " + left.key)
      builder.append(" right: " + right.key)
      if (parent != null) {
        builder.append(" parent: " + parent.key)
      }
      if (child != null) {
        builder.append(" child: " + child.key)
      }
      builder.append(" childCount: " + childCount)
      builder.append(" inHeap:" + inHeap)
      builder.toString
    }

    override def toString:String = {
      "key: "+_key+" value: "+value
    }

    private[FibonacciHeap] def clean():Unit = {
      right = this
      left = this
      parent = null
      child = null
      childCount = 0
      lostChild = false
      _key = comparator.AlwaysTop
      inHeap = false
    }

    def key:K = {
      _key
    }

    def key_(newKey:K):Unit = {
      FibonacciHeap.this.changeKey(newKey,this)
    }

    def isInHeap: Boolean = {
      inHeap
    }

    def remove():Unit = {
      FibonacciHeap.this.remove(this)
    }

    private[FibonacciHeap] def setKeyAndInHeap(key: K):Unit = {
      this._key = key
      inHeap = true
    }

    private[FibonacciHeap] def release():Unit = {
      left.right = right
      right.left = left
    }

    private[FibonacciHeap] def cat(node: HeapMember) {
      node.left = this
      node.right = right
      right = node
      node.right.left = node
    }

    private[FibonacciHeap] def addChild(childNode: HeapMember) {
      if (child == null) {
        child = childNode
        childNode.right = childNode
        childNode.left = childNode
      }
      else {
        child.cat(childNode)
      }
      childNode.parent = this
      childCount += 1
      childNode.lostChild = false
    }

    private[FibonacciHeap] def releaseChild(childNode: HeapMember) {
      childNode.release()
      childCount = childCount - 1
      if (child == childNode) {
        child = childNode.right
      }
      if (childCount == 0) {
        child = null
      }
      childNode.parent = null
      childNode.lostChild = true
    }
  }
}

object FibonacciHeap {

  trait HeapComparator[K] extends PartialOrdering[K] {

    /**
     * @throws IllegalArgumentException if the key is unusable
     */
    def checkKey(key:K)

    /**
     * A key that will always be at the top of the heap if present at all. Used to efficiently remove items from the heap.
     */
    def AlwaysTop:K
  }

  object MinDoubleHeapComparator extends HeapComparator[Double] {

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

}
