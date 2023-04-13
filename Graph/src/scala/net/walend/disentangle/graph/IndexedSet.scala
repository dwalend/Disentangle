package net.walend.disentangle.graph

import scala.collection.immutable.{Set, SetOps}
import scala.collection.mutable.{Buffer, ReusableBuilder}
import scala.collection.{IterableFactory, IterableFactoryDefaults, mutable}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */

//todo investigate backing this with a BitSet.
final class IndexedSet[A](outerSeq:IndexedSeq[A])
  extends Set[A]
    with SetOps[A,IndexedSet,IndexedSet[A]]
    with IterableFactoryDefaults[A, IndexedSet]
    with Serializable  {

  private val asSet:Set[A] = outerSeq.toSet

  if(outerSeq.size != asSet.size) throw new IllegalArgumentException(s"seq has duplicate members: ${outerSeq.groupBy(x => x).filter{(x: (A, IndexedSeq[A])) => x._2.size > 1}}")

  //Indexed access
  def apply(index:Int): A = outerSeq(index)

  def indexOf(a:A): Int = outerSeq.indexOf(a)

  def asSeq: IndexedSeq[A] = outerSeq
  
  //AbstractSet contract
  override def contains(elem: A): Boolean = asSet.contains(elem)

  override def iterator: Iterator[A] = outerSeq.iterator

  override def concat(that: collection.IterableOnce[A]): IndexedSet[A] = {
    new IndexedSet((outerSeq ++ that).distinct)
  }

  override def incl(elem: A): IndexedSet[A] = {
    if(contains(elem)) this
    else new IndexedSet(outerSeq :+ elem)
  }

  override def excl(elem: A): IndexedSet[A] = {
    if(!contains(elem)) this
    else new IndexedSet(outerSeq.filterNot(_ == elem))
  }

  override def iterableFactory: IterableFactory[IndexedSet] = IndexedSet
}


object IndexedSet extends IterableFactory[IndexedSet] {
  //  def apply[A](traversable:Traversable[A]) = IndexedSeq(traversable.to[Seq].distinct)

  def from[A](source: IterableOnce[A]): IndexedSet[A] = new IndexedSet[A](IndexedSeq.from(source))

  def empty[A]: IndexedSet[A] = new IndexedSet[A](IndexedSeq.empty)

  def newBuilder[A]: mutable.Builder[A, IndexedSet[A]] = new IndexedSetBuilderImpl[A]
}

private final class IndexedSetBuilderImpl[A] extends ReusableBuilder[A, IndexedSet[A]] {
  private val elems = mutable.ArrayBuffer.empty[A]

  override def clear(): Unit = elems.clear()

  override def result(): IndexedSet[A] = new IndexedSet(IndexedSeq.from(elems))

  def addOne(elem: A): IndexedSetBuilderImpl.this.type = {
    elems += elem
    this
  }

  override def addAll(xs: IterableOnce[A]): this.type = {
    if (xs.asInstanceOf[AnyRef] eq this) addAll(Buffer.from(xs)) // avoid mutating under our own iterator
    else {
      val it = xs.iterator
      while (it.hasNext) {
        addOne(it.next())
      }
    }
    this
  }
}