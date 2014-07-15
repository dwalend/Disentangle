package net.walend.graph

import scala.collection.generic.{ImmutableSetFactory, GenericSetTemplate}
import scala.collection.{SetLike, AbstractSet}

/**
 *
 *
 * @author dwalend
 * @since v0.1.0
 */
//todo  CustomParallelizable[A, ParIndexedSet[A]]
//  AbstractSet[A] with Set[A] with GenericSetTemplate[A, HashSet] with SetLike[A, HashSet[A]] with CustomParallelizable[A, ParHashSet[A]] with Serializable
class IndexedSet[A](seq:Seq[A]) extends AbstractSet[A] with Set[A] with GenericSetTemplate[A, IndexedSet] with SetLike[A, IndexedSet[A]] with Serializable  {

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

//extends ImmutableSetFactory[IndexedSet] with Serializable
object IndexedSet extends ImmutableSetFactory[IndexedSet] with Serializable {
  def apply[A](traversable:Traversable[A]) = IndexedSeq(traversable.to[Seq].distinct)

  def emptyInstance = new IndexedSet[Any](Seq.empty)
}