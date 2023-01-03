/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.Chunk

import scala.annotation.tailrec

/**
 * `ParSeq` is a data type that represents some notion of "events" that can
 * take place in parallel or in sequence. For example, a `ParSeq`
 * parameterized on some error type could be used to model the potentially
 * multiple ways that an application can fail. On the other hand, a ParSeq`
 * parameterized on some request type could be used to model a collection of
 * requests to external data sources, some of which could be executed in
 * parallel and some of which must be executed sequentially.
 */
sealed trait ParSeq[+Z <: Unit, +A] { self =>

  /**
   * Combines this collection of events with that collection of events to
   * return a new collection of events that represents this collection of
   * events in parallel with that collection of events.
   */
  final def &&[Z1 >: Z <: Unit, A1 >: A](that: ParSeq[Z1, A1]): ParSeq[Z1, A1] =
    ParSeq.Both(self, that)

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, B] =
    zipRight(that)

  /**
   * Combines this collection of events with that collection of events to
   * return a new collection of events that represents this collection of
   * events followed by that collection of events.
   */
  final def ++[Z1 >: Z <: Unit, A1 >: A](that: ParSeq[Z1, A1]): ParSeq[Z1, A1] =
    ParSeq.Then(self, that)

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, A] =
    zipLeft(that)

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, (A, B)] =
    zip(that)

  /**
   * Maps the events in this collection of events to the specified constant
   * value.
   */
  final def as[B](b: B): ParSeq[Z, B] =
    map(_ => b)

  /**
   * Returns the first event in this collection of events. If multiple events
   * occur in parallel and before any other events then any of these events
   * may be returned.
   */
  @tailrec
  final def first(implicit ev: Z <:< Nothing): A =
    (self: @unchecked) match {
      case ParSeq.Both(left, _) => left.first
      case ParSeq.Then(left, _) => left.first
      case ParSeq.Single(value) => value
    }

  /**
   * Constructs a new collection of events for each event in this collection of
   * events, collecting them back into a single collection of events.
   */
  final def flatMap[Z1 >: Z <: Unit, B](f: A => ParSeq[Z1, B]): ParSeq[Z1, B] =
    fold(ParSeq.empty, f)(_ ++ _, _ && _).asInstanceOf[ParSeq[Z1, B]]

  /**
   * Flattens a collection of collections of events into a single collection
   * of events.
   */
  final def flatten[Z1 >: Z <: Unit, B](implicit ev: A <:< ParSeq[Z1, B]): ParSeq[Z1, B] =
    flatMap(ev)

  /**
   * Folds over the events in this collection of events using the specified
   * functions.
   */
  final def fold[B](emptyCase: B, singleCase: A => B)(thenCase: (B, B) => B, bothCase: (B, B) => B): B = {
    @tailrec
    def loop(in: List[ParSeq[Z, A]], out: List[Either[Boolean, B]]): List[B] =
      in match {
        case ParSeq.Both(left, right) :: parSeqs => loop(left :: right :: parSeqs, Left(true) :: out)
        case ParSeq.Then(left, right) :: parSeqs => loop(left :: right :: parSeqs, Left(false) :: out)
        case ParSeq.Single(a) :: parSeqs         => loop(parSeqs, Right(singleCase(a)) :: out)
        case _ :: parSeqs                        => loop(parSeqs, Right(emptyCase) :: out)
        case Nil                                 =>
          out.foldLeft[List[B]](List.empty) {
            case (acc, Right(parSeq)) => parSeq :: acc
            case (acc, Left(true))    =>
              val left :: right :: parSeqs = (acc: @unchecked)
              bothCase(left, right) :: parSeqs
            case (acc, Left(false))   =>
              val left :: right :: parSeqs = (acc: @unchecked)
              thenCase(left, right) :: parSeqs
          }
      }
    loop(List(self), List.empty).head
  }

  /**
   * Performs the specified effectual function for each event in this
   * collection of events, collecting them back into a single collection of
   * events.
   */
  final def forEach[F[+_]: IdentityBoth: Covariant, B](f: A => F[B]): F[ParSeq[Z, B]] =
    fold[F[ParSeq[Unit, B]]](ParSeq.empty.succeed, a => f(a).map(ParSeq.single))(
      _.zipWith(_)(_ ++ _),
      _.zipWith(_)(_ && _)
    ).asInstanceOf[F[ParSeq[Z, B]]]

  final def toCause: zio.Cause[A] = this match {
    case ParSeq.Both(left, right) => zio.Cause.Both(left.toCause, right.toCause)
    case _: ParSeq.Empty.type     => zio.Cause.empty
    case ParSeq.Single(value)     => zio.Cause.fail(value)
    case ParSeq.Then(left, right) => zio.Cause.Then(left.toCause, right.toCause)
  }

  /**
   * Transforms the type of events in this collection of events with the
   * specified function.
   */
  final def map[B](f: A => B): ParSeq[Z, B] =
    flatMap(a => ParSeq.single(f(a)))

  /**
   * Converts this collection of events to a `NonEmptyMultiSet` of events,
   * discarding information about the sequential structure of events.
   */
  final def toNonEmptyMultiSet(implicit ev: Z <:< Nothing): NonEmptyMultiSet[A] =
    NonEmptyMultiSet.fromIterableOption(fold(Chunk.empty, Chunk.single)(_ ++ _, _ ++ _)).get

  /**
   * Combines this collection of events with that collection of events to
   * return the Cartesian product of events, combining the elements into a
   * tuple.
   */
  final def zip[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, (A, B)] =
    zipWith(that)((_, _))

  /**
   * Combines this collection of events with that collection of events to
   * return the Cartesian product of events, keeping only the events from this
   * collection.
   */
  final def zipLeft[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, A] =
    zipWith(that)((a, _) => a)

  /**
   * Combines this collection of events with that collection of events to
   * return the Cartesian product of events, keeping only the events from that
   * collection.
   */
  final def zipRight[Z1 >: Z <: Unit, B](that: ParSeq[Z1, B]): ParSeq[Z1, B] =
    zipWith(that)((_, b) => b)

  /**
   * Combines this collection of events with that collection of events to
   * return the Cartesian product of events using the specified function.
   */
  final def zipWith[Z1 >: Z <: Unit, B, C](that: ParSeq[Z1, B])(f: (A, B) => C): ParSeq[Z1, C] =
    self.flatMap(a => that.map(b => f(a, b)))
}

object ParSeq {

  final case class Both[+Z <: Unit, +A](left: ParSeq[Z, A], right: ParSeq[Z, A]) extends ParSeq[Z, A] { self =>
    override def equals(that: Any): Boolean                                     =
      that match {
        case that: ParSeq[Unit, Any] =>
          equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that) || commute(
            that
          ) || symmetric(empty)(self, that)
        case _                       =>
          false
      }
    override def hashCode: Int                                                  =
      ParSeq.flatten(self) match {
        case Nil                         => Empty.hashCode
        case set :: Nil if set.size == 1 => set.head.hashCode
        case seq                         => seq.hashCode
      }
    private def associate(l: ParSeq[Unit, Any], r: ParSeq[Unit, Any]): Boolean  =
      (l, r) match {
        case (Both(Both(al, bl), cl), Both(ar, Both(br, cr))) =>
          al == ar && bl == br && cl == cr
        case _                                                =>
          false
      }
    private def commute(that: ParSeq[Unit, Any]): Boolean                       =
      (self, that) match {
        case (Both(al, bl), Both(ar, br)) =>
          al == br && bl == ar
        case _                            =>
          false
      }
    private def distribute(l: ParSeq[Unit, Any], r: ParSeq[Unit, Any]): Boolean =
      (l, r) match {
        case (Both(al, bl), Then(ar, Both(br, cr))) if al == Then(ar, br) && bl == Then(ar, cr) => true
        case (Both(al, bl), Then(Both(ar, br), cr)) if al == Then(ar, cr) && bl == Then(br, cr) => true
        case _                                                                                  => false
      }
    private def equal(that: ParSeq[Unit, Any]): Boolean                         =
      (self, that) match {
        case (br: Both[Unit, Any], bl: Both[Unit, Any]) =>
          br.left == bl.left && br.right == bl.right
        case _                                          =>
          false
      }
  }

  case object Empty extends ParSeq[Unit, Nothing]

  final case class Single[+A](value: A) extends ParSeq[Nothing, A] {
    override def hashCode = value.hashCode
  }

  final case class Then[+Z <: Unit, +A](left: ParSeq[Z, A], right: ParSeq[Z, A]) extends ParSeq[Z, A] { self =>
    override def equals(that: Any): Boolean                                     = that match {
      case that: ParSeq[Unit, Any] =>
        equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that) || symmetric(empty)(
          self,
          that
        )
      case _                       => false
    }
    override def hashCode: Int                                                  =
      ParSeq.flatten(self) match {
        case Nil                         => Empty.hashCode
        case set :: Nil if set.size == 1 => set.head.hashCode
        case seq                         => seq.hashCode
      }
    private def associate(l: ParSeq[Unit, Any], r: ParSeq[Unit, Any]): Boolean  = (l, r) match {
      case (Then(Then(al, bl), cl), Then(ar, Then(br, cr))) => al == ar && bl == br && cl == cr
      case _                                                => false
    }
    private def distribute(l: ParSeq[Unit, Any], r: ParSeq[Unit, Any]): Boolean = (l, r) match {
      case (Then(al, Both(bl, cl)), Both(ar, br)) if Then(al, bl) == ar && Then(al, cl) == br => true
      case (Then(Both(al, bl), cl), Both(ar, br)) if Then(al, cl) == ar && Then(bl, cl) == br => true
      case _                                                                                  => false
    }
    private def equal(that: ParSeq[Unit, Any]): Boolean                         = (self, that) match {
      case (tl: Then[Unit, Any], tr: Then[Unit, Any]) => tl.left == tr.left && tl.right == tr.right
      case _                                          => false
    }
  }

  /**
   * Constructs a new collection of events that contains the specified event.
   */
  def apply[A](a: A): ParSeq[Nothing, A] =
    single(a)

  val empty: ParSeq[Unit, Nothing] =
    ParSeq.Empty

  /**
   * Constructs a new collection of events that contains the specified event.
   */
  def single[A](a: A): ParSeq[Nothing, A] =
    ParSeq.Single(a)

  /**
   * A collection of events that contains a single event with no information.
   */
  val unit: ParSeq[Nothing, Unit] =
    single(())

  /**
   * The `Covariant` instance for `ParSeq`.
   */
  implicit def parSeqCovariant[Z <: Unit]: Covariant[({ type lambda[+a] = ParSeq[Z, a] })#lambda] =
    new Covariant[({ type lambda[+a] = ParSeq[Z, a] })#lambda] {
      def map[A, B](f: A => B): ParSeq[Z, A] => ParSeq[Z, B] =
        _.map(f)
    }

  /**
   * Derives a `Debug[ParSeq[A]]` given a `Debug[A]`.
   */
  implicit def parSeqDebug[Z <: Unit, A: Debug]: Debug[ParSeq[Z, A]] =
    _.fold(
      Debug.Repr.Object(List("zio", "prelude"), "Empty"),
      a => Debug.Repr.VConstructor(List("zio", "prelude"), "Single", List(a.debug))
    )(
      (l, r) => Debug.Repr.VConstructor(List("zio", "prelude"), "Then", List(l, r)),
      (l, r) => Debug.Repr.VConstructor(List("zio", "prelude"), "Both", List(l, r))
    )

  /**
   * The `IdentityBoth` instance for `ParSeq`.
   */
  implicit def parSeqIdentityBoth[Z <: Unit]: IdentityBoth[({ type lambda[+a] = ParSeq[Z, a] })#lambda] =
    new IdentityBoth[({ type lambda[+a] = ParSeq[Z, a] })#lambda] {
      def any: ParSeq[Z, Any]                                                          =
        unit
      def both[A, B](left: => ParSeq[Z, A], right: => ParSeq[Z, B]): ParSeq[Z, (A, B)] =
        left.zip(right)
    }

  /**
   * The `IdentityFlatten` instance for `ParSeq`.
   */
  implicit def parSeqIdentityFlatten[Z <: Unit]: IdentityFlatten[({ type lambda[+a] = ParSeq[Z, a] })#lambda] =
    new IdentityFlatten[({ type lambda[+a] = ParSeq[Z, a] })#lambda] {
      def any: ParSeq[Z, Any]                                        =
        unit
      def flatten[A](parSeqs: ParSeq[Z, ParSeq[Z, A]]): ParSeq[Z, A] =
        parSeqs.flatten
    }

  /**
   * The `NonEmptyForEach` instance for `ParSeq.
   */
  implicit def parSeqForEach[Z <: Unit]: ForEach[({ type lambda[+a] = ParSeq[Z, a] })#lambda] =
    new ForEach[({ type lambda[+a] = ParSeq[Z, a] })#lambda] {
      def forEach[F[+_]: IdentityBoth: Covariant, A, B](fa: ParSeq[Z, A])(f: A => F[B]): F[ParSeq[Z, B]] =
        fa.forEach(f)
    }

  /**
   * The `Hash` instance for `ParSeq`. Note that due to limitations of
   * Scala's `Set` this uses object equality and hash code on the elements.
   */
  implicit def parSeqHash[Z <: Unit, A]: Hash[ParSeq[Z, A]] =
    Hash.default

  private def empty(l: ParSeq[Unit, Any], r: ParSeq[Unit, Any]): Boolean = (l, r) match {
    case (Then(a, Empty), b) => a == b
    case (Then(Empty, a), b) => a == b
    case (Both(a, Empty), b) => a == b
    case (Both(Empty, a), b) => a == b
    case _                   => false
  }

  /**
   * Flattens a parSeq to a list of sets of events, where each set
   * represents events that occur in parallel and sequential sets represent
   * events that occur in sequence.
   */
  private def flatten[Z <: Unit, A](parSeq: ParSeq[Z, A]): List[Set[A]] = {

    @tailrec
    def loop(parSeqs: List[ParSeq[Z, A]], flattened: List[Set[A]]): List[Set[A]] = {
      val (parallel, sequential) = parSeqs.foldLeft((Set.empty[A], List.empty[ParSeq[Z, A]])) {
        case ((parallel, sequential), parSeq) =>
          val (set, seq) = step(parSeq)
          (parallel ++ set, sequential ++ seq)
      }
      val updated                = if (parallel.nonEmpty) parallel :: flattened else flattened
      if (sequential.isEmpty) updated.reverse
      else loop(sequential, updated)
    }

    loop(List(parSeq), List.empty)
  }

  /**
   * Takes one step in evaluating a parSeq, returning a set of events that occur
   * in parallel and a list of events that occur sequentially after those events.
   */
  private def step[Z <: Unit, A](c: ParSeq[Z, A]): (Set[A], List[ParSeq[Z, A]]) = {

    @tailrec
    def loop(
      parSeq: ParSeq[Z, A],
      stack: List[ParSeq[Z, A]],
      parallel: Set[A],
      sequential: List[ParSeq[Z, A]]
    ): (Set[A], List[ParSeq[Z, A]]) = parSeq match {
      case Then(left, right) =>
        left match {
          case Then(l, r) =>
            loop(Then(l, Then(r, right)), stack, parallel, sequential)
          case Both(l, r) =>
            loop(Both(Then(l, right), Then(r, right)), stack, parallel, sequential)
          case other      =>
            loop(other, stack, parallel, right :: sequential)
        }
      case Both(left, right) =>
        loop(left, right :: stack, parallel, sequential)
      case Single(value)     =>
        if (stack.isEmpty) (parallel ++ Set(value), sequential)
        else loop(stack.head, stack.tail, parallel ++ Set(value), sequential)
      case _                 =>
        if (stack.isEmpty) (parallel, sequential) else loop(stack.head, stack.tail, parallel, sequential)
    }

    loop(c, List.empty, Set.empty, List.empty)
  }

  private def symmetric[Z <: Unit, A](
    f: (ParSeq[Z, A], ParSeq[Z, A]) => Boolean
  ): (ParSeq[Z, A], ParSeq[Z, A]) => Boolean =
    (l, r) => f(l, r) || f(r, l)
}
