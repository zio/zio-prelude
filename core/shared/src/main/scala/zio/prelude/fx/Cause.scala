package zio.prelude.fx

import zio.prelude._

import scala.annotation.tailrec

/**
 * `Cause` is a data type that represents the potentially multiple ways that a
 * computation can fail.
 */
sealed trait Cause[+E] { self =>

  /**
   * Combines this cause with the specified cause to return a new cause that
   * represents failure for this cause followed by failure for the specified
   * cause.
   */
  def ++[E1 >: E](that: Cause[E1]): Cause[E1] =
    Cause.Then(self, that)

  /**
   * Combines with cause with the spceified cause to return a new cause that
   * represents failure for this cause in parallel with failure for the
   * specified cause.
   */
  def &&[E1 >: E](that: Cause[E1]): Cause[E1] =
    Cause.Both(self, that)

  /**
   * Returns the first failure in this cause, discarding information on any
   * other failures that have occurred.
   */
  @tailrec
  def failure: E =
    self match {
      case Cause.Both(left, _) => left.failure
      case Cause.Then(left, _) => left.failure
      case Cause.Fail(value)   => value
    }
}

object Cause {

  final case class Both[+E](left: Cause[E], right: Cause[E]) extends Cause[E] { self =>
    override def equals(that: Any): Boolean                       =
      that match {
        case that: Cause[Any] =>
          equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that) || commute(that)
        case _                =>
          false
      }
    override def hashCode: Int                                    =
      flatten(self) match {
        case set :: Nil if set.size == 1 => set.head.hashCode
        case seq                         => seq.hashCode
      }
    private def associate(l: Cause[Any], r: Cause[Any]): Boolean  =
      (l, r) match {
        case (Both(Both(al, bl), cl), Both(ar, Both(br, cr))) =>
          al == ar && bl == br && cl == cr
        case _                                                =>
          false
      }
    private def commute(that: Cause[Any]): Boolean                =
      (self, that) match {
        case (Both(al, bl), Both(ar, br)) =>
          al == br && bl == ar
        case _                            =>
          false
      }
    private def distribute(l: Cause[Any], r: Cause[Any]): Boolean =
      (l, r) match {
        case (Both(Then(al1, bl), Then(al2, cl)), Then(ar, Both(br, cr))) =>
          al1 == al2 && al1 == ar && bl == br && cl == cr
        case (Both(Then(al, cl1), Then(bl, cl2)), Then(Both(ar, br), cr)) =>
          cl1 == cl2 && al == ar && bl == br && cl1 == cr
        case _                                                            =>
          false
      }
    private def equal(that: Cause[Any]): Boolean                  =
      (self, that) match {
        case (br: Both[Any], bl: Both[Any]) =>
          br.left == bl.left && br.right == bl.right
        case _                              =>
          false
      }
  }

  final case class Fail[+E](value: E) extends Cause[E]

  final case class Then[+E](left: Cause[E], right: Cause[E]) extends Cause[E] { self =>
    override def equals(that: Any): Boolean                       = that match {
      case that: Cause[_] =>
        equal(that) || symmetric(associate)(self, that) || symmetric(distribute)(self, that)
      case _              => false
    }
    override def hashCode: Int                                    =
      Cause.flatten(self) match {
        case set :: Nil if set.size == 1 => set.head.hashCode
        case seq                         => seq.hashCode
      }
    private def associate(l: Cause[Any], r: Cause[Any]): Boolean  = (l, r) match {
      case (Then(Then(al, bl), cl), Then(ar, Then(br, cr))) => al == ar && bl == br && cl == cr
      case _                                                => false
    }
    private def distribute(l: Cause[Any], r: Cause[Any]): Boolean = (l, r) match {
      case (Then(al, Both(bl, cl)), Both(Then(ar1, br), Then(ar2, cr)))
          if ar1 == ar2 && al == ar1 && bl == br && cl == cr =>
        true
      case (Then(Both(al, bl), cl), Both(Then(ar, cr1), Then(br, cr2)))
          if cr1 == cr2 && al == ar && bl == br && cl == cr1 =>
        true
      case _ => false
    }
    private def equal(that: Cause[Any]): Boolean                  = (self, that) match {
      case (tl: Then[_], tr: Then[_]) => tl.left == tr.left && tl.right == tr.right
      case _                          => false
    }
  }

  /**
   * Constructs a cause that represents failure with the specified value.
   */
  def fail[E](value: E): Cause[E] =
    Cause.Fail(value)

  /**
   * The `Hash` instance for `Cause`. Note that due to limitations on Scala's
   * `Set` this uses object equality and hash code on the elements.
   */
  implicit def CauseHash[E]: Hash[Cause[E]] =
    Hash.default

  private def symmetric[E](f: (Cause[E], Cause[E]) => Boolean): (Cause[E], Cause[E]) => Boolean =
    (l, r) => f(l, r) || f(r, l)

  /**
   * Flattens a cause to a sequence of sets of causes, where each set
   * represents causes that fail in parallel and sequential sets represent
   * causes that fail after each other.
   */
  private def flatten(c: Cause[_]): List[Set[Cause[_]]] = {

    @tailrec
    def loop(causes: List[Cause[_]], flattened: List[Set[Cause[_]]]): List[Set[Cause[_]]] = {
      val (parallel, sequential) = causes.foldLeft((Set.empty[Cause[_]], List.empty[Cause[_]])) {
        case ((parallel, sequential), cause) =>
          val (set, seq) = step(cause)
          (parallel ++ set, sequential ++ seq)
      }
      val updated                = if (parallel.nonEmpty) parallel :: flattened else flattened
      if (sequential.isEmpty) updated.reverse
      else loop(sequential, updated)
    }

    loop(List(c), List.empty)
  }

  /**
   * Takes one step in evaluating a cause, returning a set of causes that fail
   * in parallel and a list of causes that fail sequentially after those causes.
   */
  private def step(c: Cause[_]): (Set[Cause[_]], List[Cause[_]]) = {

    @tailrec
    def loop(
      cause: Cause[_],
      stack: List[Cause[_]],
      parallel: Set[Cause[_]],
      sequential: List[Cause[_]]
    ): (Set[Cause[_]], List[Cause[_]]) = cause match {
      case Then(left, right) =>
        left match {
          case Then(l, r) => loop(Then(l, Then(r, right)), stack, parallel, sequential)
          case Both(l, r) =>
            loop(Both(Then(l, right), Then(r, right)), stack, parallel, sequential)
          case o          => loop(o, stack, parallel, right :: sequential)
        }
      case Both(left, right) => loop(left, right :: stack, parallel, sequential)
      case o                 =>
        if (stack.isEmpty) (parallel ++ Set(o), sequential)
        else loop(stack.head, stack.tail, parallel ++ Set(o), sequential)
    }

    loop(c, List.empty, Set.empty, List.empty)
  }
}
