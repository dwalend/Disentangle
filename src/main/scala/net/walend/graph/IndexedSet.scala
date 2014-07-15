package net.walend.graph

import scala.collection.generic.{GenericParTemplate, ParSetFactory, ImmutableSetFactory, GenericSetTemplate}
import scala.collection.parallel.{ParSetLike, IterableSplitter, Combiner}
import scala.collection.parallel.immutable.ParSet
import scala.collection.{CustomParallelizable, SetLike, AbstractSet}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class IndexedSet[A](seq:Seq[A]) extends AbstractSet[A] with Set[A] with GenericSetTemplate[A, IndexedSet] with SetLike[A, IndexedSet[A]] with CustomParallelizable[A, ParIndexedSet[A]] with Serializable  {

  val asSet:Set[A] = seq.to[Set]

  import scala.collection.mutable.ArrayBuffer
  if(seq.size != asSet.size) throw new IllegalStateException(s"seq has duplicate members: ${seq.to[ArrayBuffer] -- asSet}")

  //Indexed access
  def apply(index:Int) = seq(index)

  //AbstractSet contract
  override def contains(elem: A): Boolean = asSet.contains(elem)

  override def +(elem: A): IndexedSet[A] = {
    if(asSet.contains(elem)) this
    else new IndexedSet(seq :+ elem)}

  override def -(elem: A): IndexedSet[A] = {
    if(asSet.contains(elem)) {
      new IndexedSet(seq.filter(_!=elem))
    }
    else this
  }

  override def iterator: Iterator[A] = seq.iterator

  //GenericSetTemplate
  override def companion = IndexedSet

}

object IndexedSet extends ImmutableSetFactory[IndexedSet] with Serializable {
//  def apply[A](traversable:Traversable[A]) = IndexedSeq(traversable.to[Seq].distinct)

  lazy val emptyInstance = new IndexedSet[Any](Seq.empty)
}

//extends ParSet[T] with GenericParTemplate[T, ParHashSet] with ParSetLike[T, ParHashSet[T], HashSet[T]] with Serializable
class ParIndexedSet[A](indexedSet:IndexedSet[A]) extends GenericParTemplate[A, ParIndexedSet] with ParSet[A] with ParSetLike[A, ParIndexedSet[A], IndexedSet[A]] with Serializable {
  //ParSet methods
  //ParIterableLike methods
  override def seq: IndexedSet[A] = indexedSet

//  override protected[parallel] def splitter: IterableSplitter[A] = ???
  //seq.par.splitter is protected, but iterator is not. However, iterator just returns splitter. casting.
  override def splitter: IterableSplitter[A] = indexedSet.seq.par.iterator.asInstanceOf[IterableSplitter[A]]

  //GenSetLike methods
  override def +(elem: A): ParIndexedSet[A] = new ParIndexedSet(indexedSet + elem)

  override def contains(elem: A): Boolean = indexedSet.contains(elem)

  override def -(elem: A): ParIndexedSet[A] = new ParIndexedSet(indexedSet - elem)

  //todo I'm surprised I had to put this here. Worked in IndexedSet without it.
  override def empty = new ParIndexedSet[A](new IndexedSet(Seq.empty))

  //GenTraversableLike
  override def size: Int = indexedSet.size

  //GenericParTemplate
  override def companion = ParIndexedSet
}

//extends ParSetFactory[ParHashSet] with Serializable
object ParIndexedSet extends ParSetFactory[ParIndexedSet] {
  //ParSetFactory methods
  override def newCombiner[A]: Combiner[A, ParIndexedSet[A]] = ???

  def emptyInstance = new ParIndexedSet(IndexedSet.emptyInstance)
}