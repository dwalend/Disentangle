package net.walend.graph

import scala.collection.generic._
import scala.collection.parallel.{ParSetLike, IterableSplitter, Combiner}
import scala.collection.parallel.immutable.ParSet
import scala.collection.{mutable, CustomParallelizable, SetLike, AbstractSet}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
class IndexedSet[A](outerSeq:IndexedSeq[A]) extends AbstractSet[A] with Set[A] with GenericSetTemplate[A, IndexedSet] with SetLike[A, IndexedSet[A]] with CustomParallelizable[A, ParIndexedSet[A]] with Serializable  {

  lazy val asSet:Set[A] = outerSeq.to[Set]

  import scala.collection.mutable.ArrayBuffer
  if(outerSeq.size != asSet.size) throw new IllegalStateException(s"seq has duplicate members: ${outerSeq.to[ArrayBuffer] -- asSet}")

  //Indexed access
  def apply(index:Int) = outerSeq(index)

  def indexOf(a:A) = outerSeq.indexOf(a)

  def asSeq = outerSeq
  
  //AbstractSet contract
  override def contains(elem: A): Boolean = asSet.contains(elem)

  override def +(elem: A): IndexedSet[A] = {
    if(asSet.contains(elem)) this
    else new IndexedSet(outerSeq :+ elem)}

  override def -(elem: A): IndexedSet[A] = {
    if(asSet.contains(elem)) {
      new IndexedSet(outerSeq.filter(_!=elem))
    }
    else this
  }

  override def iterator: Iterator[A] = outerSeq.iterator

  //GenericSetTemplate
  override def companion = IndexedSet
}

object IndexedSet extends ImmutableSetFactory[IndexedSet] with Serializable {
//  def apply[A](traversable:Traversable[A]) = IndexedSeq(traversable.to[Seq].distinct)

  lazy val emptyInstance = new IndexedSet[Any](IndexedSeq.empty)

  override def newBuilder[A]:mutable.Builder[A,IndexedSet[A]] = {
    new mutable.SetBuilder[A,IndexedSet[A]](IndexedSet.empty[A])
  }

  implicit def canBuildFrom[T]: CanBuildFrom[IndexedSet[_], T, IndexedSet[T]] =
    new CanBuildFrom[IndexedSet[_], T, IndexedSet[T]] {
      def apply(from: IndexedSet[_]) = newBuilder[T]
      def apply() = newBuilder[T]
    }
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
  override def empty = new ParIndexedSet[A](new IndexedSet(IndexedSeq.empty))

  //GenTraversableLike
  override def size: Int = indexedSet.size

  //GenericParTemplate
  override def companion = ParIndexedSet
}

//extends ParSetFactory[ParHashSet] with Serializable
object ParIndexedSet extends ParSetFactory[ParIndexedSet] {
  //ParSetFactory methods
  //todo
  override def newCombiner[A]: Combiner[A, ParIndexedSet[A]] = ???

  def emptyInstance = new ParIndexedSet(IndexedSet.emptyInstance)
}