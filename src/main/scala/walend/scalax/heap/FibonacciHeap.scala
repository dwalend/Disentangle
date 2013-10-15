package walend.scalax.heap

/**
 * A generic Fibonacci heap
 *
 * @author dwalend
 * @since 10/14/13 10:17 AM
 */

//todo custom Ordering with magic top value
//todo make generic key
class FibonacciHeap[V] //todo implement heap
{
  def isEmpty:Boolean = {
    size==0
  }

  def insert(key:Double,value:V):HeapMember = {
    checkKeyValue(key)

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
  
  def topKey:Double = {
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

  def changeKey(key:Double,fibNode:HeapMember):Unit = {
    checkKeyValue(key)
    if(key > fibNode.key) {
      remove(fibNode)
      reinsert(key,fibNode)
    }
    else {
      decreaseKey(key,fibNode)
    }
  }

  def remove(fibNode:HeapMember):Unit = {
    decreaseKey(-Double.MaxValue,fibNode)
    takeTop()
  }

  //    public boolean contains(DoubleHeapMember node);
  /*
  public void clear()
  {
      top=null;
      size=0;
  }
  */

  override def toString: String = {
    val builder = new StringBuilder()

    builder.append("FibDoubleHeap size: "+size+"\n")
    builder.append("top: ")
    if(top==null)
    {
      builder.append("null")
    }
    else
    {
      builder.append(top.toString())
    }
    builder.append("\n")

    val it:ChildIterator = iterator()
    while(it.hasNext)
    {
      builder.append(it.next().toString)
      builder.append("\n")
    }
    builder.toString()
  }

  private var top:HeapMember = null
  private var size:Int = 0

  private def reinsert(key:Double,fibNode:HeapMember):HeapMember = {
    checkKeyValue(key)

    fibNode.setKeyAndInHeap(key)

    if(top != null) {
      top.cat(fibNode)
    }
    if((top==null)||(fibNode.key<top.key)) {
      top = fibNode
    }
    size = size +1

    fibNode
  }

  /*
  public FibDoubleHeap(DoubleHeap heap)
  {
      //todo union
      //        union(heap);
  }
  */

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
      }//do
      while(x!=top)
  
      while(rootCount>0) {
        var d:Int = x.childCount

        val next:HeapMember = x.right
        while(fibNodes(d) != null) {
          var y:HeapMember = fibNodes(d)
          if(x.key>y.key) {
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
            if(fibNodes(i).key < top.key){
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

  private def checkKeyValue(key:Double):Unit = {
    if(!(key > -Double.MaxValue)) {
      throw new IllegalArgumentException("key is "+key+" but must be greater than "+ -Double.MaxValue)
    }
  }

  private def checkTop():Unit = {
    if(top==null) {
      throw new IllegalStateException("The heap is empty.")
    }
  }

  private def decreaseKey(key:Double,fibNode:HeapMember):Unit = {
    fibNode.setKeyAndInHeap(key)
    val y:HeapMember = fibNode.parent
    if((y!=null)&&(fibNode.key<y.key)){
      cut(fibNode,y)
      cascadingCut(y)
    }
    if(fibNode.key<top.key){
      top = fibNode
    }
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

  //todo move changeKey and remove into here
  class HeapMember(val value:V) {

    private var _key: Double = .0
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
      _key = 0
      inHeap = false
    }

    //todo standard accessors
    def key: Double = {
      _key
    }

    def isInHeap: Boolean = {
      inHeap
    }

    private[FibonacciHeap] def setKeyAndInHeap(key: Double):Unit = {
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
      childCount -= 1
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

//todo ordering needs go here

}
