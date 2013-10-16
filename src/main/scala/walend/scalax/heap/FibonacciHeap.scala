package walend.scalax.heap

/**
 * A generic Fibonacci heap
 *
 * @author dwalend
 * @since 10/14/13 10:17 AM
 */

class FibonacciHeap[K,V](comparator:HeapOrdering[K]) extends Heap[K,V]
{

  def isEmpty:Boolean = size==0

  def insert(key:K,value:V):FibonacciHeapMember = {
    comparator.checkKey(key)

    val fibNode:FibonacciHeapMember = new FibonacciHeapMember(value)

    reinsert(key,fibNode)
  }

  def topMember:FibonacciHeapMember = {
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

  def takeTop():FibonacciHeapMember = {
    checkTop()
    val z:FibonacciHeapMember = top
    while(z.child!=null) {
      val x:FibonacciHeapMember = z.child
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

  private def changeKey(key:K,fibNode:FibonacciHeapMember):Unit = {
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

  private def remove(fibNode:FibonacciHeapMember):Unit = {
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
  //todo members

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

  private var top:FibonacciHeapMember = null
  private var size:Int = 0

  private def reinsert(key:K,fibNode:FibonacciHeapMember):FibonacciHeapMember = {
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

  private def cascadingCut(y: FibonacciHeapMember): Unit = {
    val z: FibonacciHeapMember = y.parent
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
    val fibNodes:ArraySeq[FibonacciHeapMember] = new ArraySeq[FibonacciHeapMember](size)

    var rootCount:Int = 0
    var x:FibonacciHeapMember = top
    if(x!=null) {
      do {
        rootCount = rootCount + 1
        x = x.right
      } while(x!=top)
  
      while(rootCount>0) {
        var d:Int = x.childCount

        val next:FibonacciHeapMember = x.right
        while(fibNodes(d) != null) {
          var y:FibonacciHeapMember = fibNodes(d)
          if(comparator.gt(x.key,y.key)) {
            val temp:FibonacciHeapMember = y
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
  private def cut(x:FibonacciHeapMember,y:FibonacciHeapMember):Unit = {
    y.releaseChild(x)
    top.cat(x)
  }

  /**
  Make y a child of x.
    */
  private def link(y:FibonacciHeapMember,x:FibonacciHeapMember):Unit = {
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

  private def raiseKey(key:K,fibNode:FibonacciHeapMember):Unit = {
    fibNode.setKeyAndInHeap(key)
    val y:FibonacciHeapMember = fibNode.parent
    if((y!=null) && comparator.lt(fibNode.key,y.key)) {
          cut(fibNode,y)
          cascadingCut(y)
    }
    if(comparator.lt(fibNode.key,top.key)) top = fibNode
  }

  private class ChildIterator(startNode:FibonacciHeapMember) {
  
    private var currentNode:FibonacciHeapMember = null
    private var currentChildIterator:ChildIterator = null
    
    def hasNext:Boolean = {
      if((currentChildIterator!=null)&&(currentChildIterator.hasNext)) {
        return true
      }
      startNode != currentNode
    }
    
    def next():FibonacciHeapMember = {
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

  class FibonacciHeapMember(val value:V) extends HeapMember {

    private var _key:K = comparator.AlwaysTop
    private[FibonacciHeap] var parent: FibonacciHeapMember = null //todo only need accessor outside of member
    private[FibonacciHeap] var child: FibonacciHeapMember = null //todo only need accessor outside of member
    private var left: FibonacciHeapMember = this
    private[FibonacciHeap] var right: FibonacciHeapMember = this //todo only need accessor outside of member
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

    private[FibonacciHeap] def cat(node: FibonacciHeapMember) {
      node.left = this
      node.right = right
      right = node
      node.right.left = node
    }

    private[FibonacciHeap] def addChild(childNode: FibonacciHeapMember) {
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

    private[FibonacciHeap] def releaseChild(childNode: FibonacciHeapMember) {
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

}
