package zio.prelude

import scala.annotation.tailrec

/**
 * `Semiring` is a data type that represents some notion of "events" that can
 * take place in parallel or in sequence. For example, a semiring
 * parameterized on some error type could be used to model the potentially
 * multiple ways that an application can fail. On the other hand, a semiring
 * parameterized on some request type could be used to model a collection of
 * requests to external data sources, some of which could be executed in
 * parallel and some of which must be executed sequentially.
 */
sealed trait Semiring[+A] { self =>

  /**
   * Combines this collection of events with that collection of events to
   * return a new collection of events that represents this collection of
   * events followed by that collection of events.
   */
  def ++[A1 >: A](that: Semiring[A1]): Semiring[A1] =
    Semiring.Then(self, that)

  /**
   * Combines this collection of events with that collection of events to
   * return a new collection of events that represents this collection of
   * events in parallel with that collection of events.
   */
  def &&[A1 >: A](that: Semiring[A1]): Semiring[A1] =
    Semiring.Both(self, that)

  /**
   * Returns the first event in this collection of events. If multiple events
   * occur in parallel and before any other events then any of these events
   * may be returned.
   */
  @tailrec
  final def first: A =
    self match {
      case Semiring.Both(left, _) => left.first
      case Semiring.Then(left, _) => left.first
      case Semiring.Single(value) => value
    }
}

object Semiring {

  final case class Both[+A](left: Semiring[A], right: Semiring[A]) extends Semiring[A] { self =>
    override def equals(that: Any): Boolean                             =
      that match {
        case that: Semiring[Any] =>
          equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that) || commute(that)
        case _                   =>
          false
      }
    override def hashCode: Int                                          =
      flatten(self).hashCode
    private def associate(l: Semiring[Any], r: Semiring[Any]): Boolean  =
      (l, r) match {
        case (Both(Both(al, bl), cl), Both(ar, Both(br, cr))) =>
          al == ar && bl == br && cl == cr
        case _                                                =>
          false
      }
    private def commute(that: Semiring[Any]): Boolean                   =
      (self, that) match {
        case (Both(al, bl), Both(ar, br)) =>
          al == br && bl == ar
        case _                            =>
          false
      }
    private def distribute(l: Semiring[Any], r: Semiring[Any]): Boolean =
      (l, r) match {
        case (Both(Then(al1, bl), Then(al2, cl)), Then(ar, Both(br, cr))) =>
          al1 == al2 && al1 == ar && bl == br && cl == cr
        case (Both(Then(al, cl1), Then(bl, cl2)), Then(Both(ar, br), cr)) =>
          cl1 == cl2 && al == ar && bl == br && cl1 == cr
        case _                                                            =>
          false
      }
    private def equal(that: Semiring[Any]): Boolean                     =
      (self, that) match {
        case (br: Both[Any], bl: Both[Any]) =>
          br.left == bl.left && br.right == bl.right
        case _                              =>
          false
      }
  }

  final case class Single[+A](value: A) extends Semiring[A]

  final case class Then[+A](left: Semiring[A], right: Semiring[A]) extends Semiring[A] { self =>
    override def equals(that: Any): Boolean                             = that match {
      case that: Semiring[_] =>
        equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that)
      case _                 => false
    }
    override def hashCode: Int                                          =
      flatten(self).hashCode
    private def associate(l: Semiring[Any], r: Semiring[Any]): Boolean  = (l, r) match {
      case (Then(Then(al, bl), cl), Then(ar, Then(br, cr))) => al == ar && bl == br && cl == cr
      case _                                                => false
    }
    private def distribute(l: Semiring[Any], r: Semiring[Any]): Boolean = (l, r) match {
      case (Then(al, Both(bl, cl)), Both(Then(ar1, br), Then(ar2, cr)))
          if ar1 == ar2 && al == ar1 && bl == br && cl == cr =>
        true
      case (Then(Both(al, bl), cl), Both(Then(ar, cr1), Then(br, cr2)))
          if cr1 == cr2 && al == ar && bl == br && cl == cr1 =>
        true
      case _ => false
    }
    private def equal(that: Semiring[Any]): Boolean                     = (self, that) match {
      case (tl: Then[_], tr: Then[_]) => tl.left == tr.left && tl.right == tr.right
      case _                          => false
    }
  }

  /**
   * Constructs a new collection of events that contains the specified event.
   */
  def apply[A](a: A): Semiring[A] =
    single(a)

  /**
   * Constructs a new collection of events that contains the specified event.
   */
  def single[A](a: A): Semiring[A] =
    Semiring.Single(a)

  /**
   * The `Hash` instance for `Semiring`. Note that due to limitations of
   * Scala's `Set` this uses object equality and hash code on the elements.
   */
  implicit def SemiringHash[A]: Hash[Semiring[A]] =
    Hash.default

  /**
   * Flattens a semiring to a list of sets of events, where each set
   * represents events that occur in parallel and sequential sets represent
   * events that occur in sequence.
   */
  private def flatten[A](semiring: Semiring[A]): List[Set[A]] = {

    @tailrec
    def loop[A](semirings: List[Semiring[A]], flattened: List[Set[A]]): List[Set[A]] = {
      val (parallel, sequential) = semirings.foldLeft((Set.empty[A], List.empty[Semiring[A]])) {
        case ((parallel, sequential), semiring) =>
          val (set, seq) = step(semiring)
          (parallel ++ set, sequential ++ seq)
      }
      val updated                = if (parallel.nonEmpty) parallel :: flattened else flattened
      if (sequential.isEmpty) updated.reverse
      else loop(sequential, updated)
    }

    loop(List(semiring), List.empty)
  }

  /**
   * Takes one step in evaluating a semiring, returning a set of events that occur
   * in parallel and a list of events that occur sequentially after those events.
   */
  private def step[A](c: Semiring[A]): (Set[A], List[Semiring[A]]) = {

    @tailrec
    def loop(
      semiring: Semiring[A],
      stack: List[Semiring[A]],
      parallel: Set[A],
      sequential: List[Semiring[A]]
    ): (Set[A], List[Semiring[A]]) = semiring match {
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
    }

    loop(c, List.empty, Set.empty, List.empty)
  }

  private def symmetric[A](f: (Semiring[A], Semiring[A]) => Boolean): (Semiring[A], Semiring[A]) => Boolean =
    (l, r) => f(l, r) || f(r, l)
}
