package scala.net.walend.disentangle.graph.par

import net.walend.disentangle.graph.IndexedSet

import scala.collection.generic.{CanCombineFrom, GenericParCompanion, GenericParTemplate, ParSetFactory}
import scala.collection.immutable.{Set, SetOps}
import scala.collection.mutable.{Buffer, ReusableBuilder}
import scala.collection.parallel.immutable.{ParSeq, ParSet}
import scala.collection.parallel.{Combiner, IterableSplitter, ParIterable, ParSetLike, SeqSplitter}
import scala.collection.{IterableFactory, IterableFactoryDefaults, mutable}

/**
 * A parallel indexed set
 *
 * @author dwalend
 * @since v0.3.0
 */

final class ParIndexedSet[A](outerSeq:ParSeq[A])
  extends GenericParTemplate[A, ParIndexedSet]
    with scala.collection.parallel.ParSet[A]
    with ParIterable[A]
    with ParSetLike[A, ParIndexedSet, ParIndexedSet[A], scala.collection.immutable.Set[A]] {

  private val asSet:ParSet[A] = outerSeq.toSet

  override def contains(elem: A): Boolean = asSet.contains(elem)

  override def +(elem: A): ParIndexedSet[A] = {
    if (contains(elem)) this
    else new ParIndexedSet(outerSeq :+ elem)
  }

  override def -(elem: A): ParIndexedSet[A] = {
    if (!contains(elem)) this
    else new ParIndexedSet(outerSeq.filterNot(_ == elem))
  }

  override def size: Int = asSet.size

  override def seq: IndexedSet[A] = new IndexedSet[A](IndexedSeq.from(outerSeq))

  override def splitter: IterableSplitter[A] = ???//SeqSplitter[A]

  override def empty: ParIndexedSet[A] = new ParIndexedSet(ParSeq.empty[A])

  override def companion: GenericParCompanion[ParIndexedSet] = ParIndexedSet

  override def stringPrefix = "ParIndexedSet"

}

object ParIndexedSet extends ParSetFactory[ParIndexedSet] {
  def newCombiner[T]: Combiner[T, ParIndexedSet[T]] = ???

  implicit def canBuildFrom[S, T]: CanCombineFrom[ParIndexedSet[S], T, ParIndexedSet[T]] = new GenericCanCombineFrom[S, T]
}