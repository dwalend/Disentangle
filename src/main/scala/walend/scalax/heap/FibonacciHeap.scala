package walend.scalax.heap

/**
 * A generic Fibonacci heap
 *
 * @author dwalend
 * @since 10/14/13 10:17 AM
 */

//todo make generic key
//todo make generic value
//todo custom comparator with magic lowest value
class FibonacciHeap //todo implement heap
{
  def isEmpty:Boolean = {
    size==0
  }

  def insert(key:Double,value:Any):HeapMember = {
    checkKeyValue(key)

    val fibNode:HeapMember = new HeapMember(value)

    reinsert(key,fibNode)
  }

  def topMember:HeapMember = {
    checkTop()
    top
  }

  def topValue:Any = {
    topMember.value
  }
  
  def topKey:Double = {
    checkTop()
    top.getKey
  }

  def takeTop():HeapMember = {
    checkTop()
    val z:HeapMember = top
    while(z.getChild(this)!=null) {
      val x:HeapMember = z.getChild(this)
      z.removeChild(x,this)
      z.cat(x,this)
    }
    z.remove(this)

    if(z==z.getRight(this)) {
      top = null
    }
    else {
      top = z.getRight(this)
      consolidate()
    }

    size = size -1
    z.clean()
    z
  }

  def changeKey(key:Double,fibNode:HeapMember):Unit = {
    checkKeyValue(key)
    if(key > fibNode.getKey) {
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

    fibNode.setKey(key,this)

    if(top != null) {
      top.cat(fibNode,this)
    }
    if((top==null)||(fibNode.getKey<top.getKey)) {
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
    val z: HeapMember = y.getParent(this)
    if (z != null) {
      if (!y.lostChild(this)) {
        y.setLostChild(lostChild = true, this)
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
        x = x.getRight(this)
      }//do
      while(x!=top)
  
      while(rootCount>0) {
        var d:Int = x.getChildCount(this)

        val next:HeapMember = x.getRight(this)
        while(fibNodes(d) != null) {
          var y:HeapMember = fibNodes(d)
          if(x.getKey>y.getKey) {
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
            fibNodes(i).remove(this)
            top.cat(fibNodes(i),this)
            if(fibNodes(i).getKey < top.getKey){
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
    y.removeChild(x,this)
    top.cat(x,this)
  }

  /**
  Make y a child of x.
    */
  private def link(y:HeapMember,x:HeapMember):Unit = {
    //remove y from the list of the heap
    y.remove(this)
    
    //make y a child of x
    x.addChild(y,this)
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
    fibNode.setKey(key,this)
    val y:HeapMember = fibNode.getParent(this)
    if((y!=null)&&(fibNode.getKey<y.getKey)){
      cut(fibNode,y)
      cascadingCut(y)
    }
    if(fibNode.getKey<top.getKey){
      top = fibNode
    }
  }

  private class ChildIterator(startNode:HeapMember,heap:FibonacciHeap) {
  
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
        currentNode = currentNode.getRight(heap)
        if(currentNode.getChild(heap)!=null){
          currentChildIterator = new ChildIterator(currentNode.getChild(heap),heap)
        }
        oldCurrentNode
      }
      else
      {
        currentNode = startNode.getRight(heap)
        if(startNode.getChild(heap)!=null) {
          currentChildIterator = new ChildIterator(startNode.getChild(heap),heap)
        }
        startNode
      }
    }
    
    def remove():Unit = throw new UnsupportedOperationException
  }

  private def iterator():ChildIterator = {
    new ChildIterator(top,this)
  }

  class HeapMember(val value:Any) {

    private var key: Double = .0
    private var parent: HeapMember = null
    private var child: HeapMember = null
    private var left: HeapMember = this
    private var right: HeapMember = this
    private var childCount: Int = 0
    private var lostChild: Boolean = false
    private var inHeap: Boolean = false

    def toDebugString: String = {
      val builder: StringBuffer = new StringBuffer
      builder.append("key: " + key + "value: "+value)
      builder.append(" lostChild: " + lostChild)
      builder.append(" left: " + left.getKey)
      builder.append(" right: " + right.getKey)
      if (parent != null) {
        builder.append(" parent: " + parent.getKey)
      }
      if (child != null) {
        builder.append(" child: " + child.getKey)
      }
      builder.append(" count: " + childCount)
      builder.append(" inHeap:" + inHeap)
      builder.toString
    }

    override def toString:String = {
      "key: "+key+" value: "+value
    }

    def clean():Unit = {
      setRight(this)
      setLeft(this)
      parent = null
      child = null
      childCount = 0
      lostChild = false
      key = 0
      inHeap = false
    }

    def getKey: Double = {
      key
    }

    private def validateHeap(heap: FibonacciHeap):Unit = {
      if (heap == null) {
        throw new NullPointerException("These methods should only be called by an DoubleHeap.")
      }
    }

    def isInHeap: Boolean = {
      inHeap
    }

    def setKey(key: Double, heap: FibonacciHeap):Unit = {
      validateHeap(heap)
      this.key = key
      inHeap = true
    }

    private def remove():Unit = {
      getLeft.setRight(getRight)
      getRight.setLeft(getLeft)
    }

    def remove(heap: FibonacciHeap):Unit = {
      validateHeap(heap)
      remove()
    }

    private def cat(node: HeapMember) {
      node.setLeft(this)
      node.setRight(getRight)
      setRight(node)
      node.getRight.setLeft(node)
    }

    def cat(node: HeapMember, heap: FibonacciHeap) {
      validateHeap(heap)
      cat(node)
    }

    def addChild(childNode: HeapMember, heap: FibonacciHeap) {
      validateHeap(heap)
      if (getChild == null) {
        setChild(childNode)
        childNode.setRight(childNode)
        childNode.setLeft(childNode)
      }
      else {
        getChild.cat(childNode)
      }
      childNode.setParent(this)
      childCount += 1
      childNode.setLostChild(lostChild = false)
    }

    def removeChild(childNode: HeapMember, heap: FibonacciHeap) {
      validateHeap(heap)
      childNode.remove()
      childCount -= 1
      if (getChild eq childNode) {
        setChild(childNode.getRight)
      }
      if (getChildCount == 0) {
        setChild(null)
      }
      childNode.setParent(null)
      childNode.setLostChild(lostChild = true)
    }

    def getParent(heap: FibonacciHeap): HeapMember = {
      validateHeap(heap)
      parent
    }

    private def setParent(parent: HeapMember) {
      this.parent = parent
    }

    private def getLeft: HeapMember = {
      left
    }

    private def setLeft(left: HeapMember) {
      this.left = left
    }

    def getLeft(heap: FibonacciHeap): HeapMember = {
      validateHeap(heap)
      left
    }

    private def getRight: HeapMember = {
      right
    }

    private def setRight(right: HeapMember) {
      this.right = right
    }

    def getRight(heap: FibonacciHeap): HeapMember = {
      validateHeap(heap)
      right
    }

    private def getChild: HeapMember = {
      child
    }

    private def setChild(child: HeapMember) {
      this.child = child
    }

    def getChild(heap: FibonacciHeap): HeapMember = {
      validateHeap(heap)
      child
    }

    private def getChildCount: Int = {
      childCount
    }

    def getChildCount(heap: FibonacciHeap): Int = {
      validateHeap(heap)
      childCount
    }

    def lostChild(heap: FibonacciHeap): Boolean = {
      validateHeap(heap)
      lostChild
    }

    private def setLostChild(lostChild: Boolean) {
      this.lostChild = lostChild
    }

    def setLostChild(lostChild: Boolean, heap: FibonacciHeap) {
      validateHeap(heap)
      setLostChild(lostChild)
    }
  }
}

