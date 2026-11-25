package zio.prelude.data

import zio.Chunk
import zio.prelude.IterableOnceCompat

import scala.language.implicitConversions

/**
 * Custom option type to be used for wrapping external data models where most of the fields are
 * defined to be optional.
 *
 * Instances of Optional are either [[Optional.Present]] or [[Optional.Absent]].
 *
 * Its operators mirror those of [[scala.Option]]. The main difference is that there is an implicit
 * conversion from `A` to `Optional[A]`, and from `Option[A]` to `Optional[A]`, so a field of type
 * `Optional[A]` accepts a plain value, an `Option`, or `Optional.Absent`. A `null` value of type `A`,
 * such as a `String` variable that holds `null`, converts to `Present(null)`, not to `Absent`. A `null`
 * literal is not converted: it is already a value of type `Optional[A]`, so the field holds `null`.
 *
 * On Scala 2.13 and Scala 3 an `Optional` is also an `IterableOnce`, so it can be returned from the
 * function passed to a collection's `flatMap`.
 *
 * {{{
 * final case class Profile(nickname: Optional[String], age: Optional[Int])
 *
 * val knownAge: Option[Int] = Some(30)
 *
 * Profile("zed", knownAge)
 * // Profile(Present(zed),Present(30))
 *
 * Profile(Optional.Absent, None).age.getOrElse(0)
 * // 0
 * }}}
 */
sealed trait Optional[+A] extends IterableOnceCompat[A] { self =>

  /** Returns `true` if this is `Absent`. */
  def isEmpty: Boolean

  /** Returns `true` if this is `Present`. */
  def isDefined: Boolean

  /** Returns `true` if this is `Present`. */
  def nonEmpty: Boolean

  /**
   * Converts this optional value to standard [[scala.Option]]: `Present(a)` becomes `Some(a)`
   * and `Absent` becomes `None`.
   */
  final def toOption: Option[A] = self match {
    case Optional.Present(get) => Some(get)
    case Optional.Absent       => None
  }

  /** Returns the value if it is present, or evaluates and returns `default` if it is absent. */
  final def getOrElse[A0 >: A](default: => A0): A0 =
    self match {
      case Optional.Present(get) => get
      case Optional.Absent       => default
    }

  /** Applies `f` to the value if it is present, wrapping the result in `Present`, and returns `Absent` otherwise. */
  final def map[B](f: A => B): Optional[B] =
    self match {
      case Optional.Present(get) => Optional.Present(f(get))
      case Optional.Absent       => Optional.Absent
    }

  /** Returns the result of applying `f` to the value if it is present, and `Absent` otherwise. */
  final def flatMap[B](f: A => Optional[B]): Optional[B] =
    self match {
      case Optional.Present(get) => f(get)
      case Optional.Absent       => Optional.Absent
    }

  /** Returns `f` applied to the value if it is present, or evaluates `ifEmpty` if it is absent. */
  final def fold[B](ifEmpty: => B)(f: A => B): B =
    self match {
      case Optional.Present(get) => f(get)
      case Optional.Absent       => ifEmpty
    }

  /** Removes one level of nesting: returns the inner `Optional` if this is present, and `Absent` otherwise. */
  final def flatten[B](implicit ev: A <:< Optional[B]): Optional[B] =
    self match {
      case Optional.Present(get) => ev(get)
      case Optional.Absent       => None
    }

  /** Returns `Left` with the value if it is present, and `Right(right)` if it is absent. */
  final def toLeft[R](right: R): Either[A, R] =
    self match {
      case Optional.Present(get) => Left(get)
      case Optional.Absent       => Right(right)
    }

  /** Returns `Right` with the value if it is present, and `Left(left)` if it is absent. */
  final def toRight[L](left: L): Either[L, A] =
    self match {
      case Optional.Present(get) => Right(get)
      case Optional.Absent       => Left(left)
    }

  /** Returns this value if it is present and satisfies `p`, and `Absent` otherwise. */
  final def filter(p: A => Boolean): Optional[A] =
    self match {
      case Optional.Present(get) => if (p(get)) this else Optional.Absent
      case Optional.Absent       => Optional.Absent
    }

  /** Returns this value if it is present and does not satisfy `p`, and `Absent` otherwise. */
  final def filterNot(p: A => Boolean): Optional[A] =
    self match {
      case Optional.Present(get) => if (!p(get)) this else Optional.Absent
      case Optional.Absent       => Optional.Absent
    }

  /**
   * Returns a filtered view of this value that supports `map`, `flatMap`, `foreach` and
   * `withFilter`, so `if` guards can be used in `for` comprehensions over an `Optional`.
   */
  final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** A view of an `Optional` filtered by `p`, used by `if` guards in `for` comprehensions. */
  class WithFilter(p: A => Boolean) {

    /** Filters the underlying value with `p`, then maps it with `f`. */
    def map[B](f: A => B): Optional[B] = self filter p map f

    /** Filters the underlying value with `p`, then flat maps it with `f`. */
    def flatMap[B](f: A => Optional[B]): Optional[B] = self filter p flatMap f

    /** Filters the underlying value with `p`, then runs `f` on it if it remains. */
    def foreach[U](f: A => U): Unit = self filter p foreach f

    /** Returns a view filtered by both `p` and `q`. */
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /** Returns `true` if this value is present and equal to `elem` according to `==`. */
  final def contains[A1 >: A](elem: A1): Boolean =
    self match {
      case Optional.Present(get) => get == elem
      case Optional.Absent       => false
    }

  /** Returns `true` if this value is present and satisfies `p`. */
  final def exists(p: A => Boolean): Boolean =
    self match {
      case Optional.Present(get) => p(get)
      case Optional.Absent       => false
    }

  /** Returns `true` if this value is absent, or if it is present and satisfies `p`. */
  final def forall(p: A => Boolean): Boolean =
    self match {
      case Optional.Present(get) => p(get)
      case Optional.Absent       => true
    }

  /** Runs `f` on the value if it is present, and does nothing otherwise. */
  final def foreach[U](f: A => U): Unit =
    self match {
      case Optional.Present(get) => val _ = f(get)
      case Optional.Absent       =>
    }

  /**
   * Returns the result of `pf` if this value is present and `pf` is defined at it, and `Absent`
   * otherwise.
   */
  final def collect[B](pf: PartialFunction[A, B]): Optional[B] =
    self match {
      case Optional.Present(get) => pf.lift(get)
      case Optional.Absent       => Optional.Absent
    }

  /** Returns this value if it is present, and evaluates and returns `alternative` otherwise. */
  final def orElse[B >: A](alternative: => Optional[B]): Optional[B] =
    if (isEmpty) alternative else this

  /** Returns an iterator over the value if it is present, or an empty iterator otherwise. */
  override final def iterator: Iterator[A] =
    self match {
      case Optional.Present(get) => Iterator.single(get)
      case Optional.Absent       => Iterator.empty
    }

  /** Returns a `Chunk` with the value if it is present, or an empty `Chunk` otherwise. */
  final def toChunk: Chunk[A] =
    self match {
      case Optional.Present(get) => Chunk.single(get)
      case Optional.Absent       => Chunk.empty
    }

  /** Returns a `List` with the value if it is present, or `Nil` otherwise. */
  final def toList: List[A] =
    self match {
      case Optional.Present(get) => get :: Nil
      case Optional.Absent       => Nil
    }

  /** Returns a `Vector` with the value if it is present, or an empty `Vector` otherwise. */
  final def toVector: Vector[A] =
    self match {
      case Optional.Present(get) => Vector(get)
      case Optional.Absent       => Vector.empty
    }

  /**
   * Returns the number of elements: 1 if the value is present and 0 if it is absent.
   */
  override final def knownSize: Int = if (isEmpty) 0 else 1

}

/**
 * The two cases of [[Optional]] and the implicit conversions to it.
 */
object Optional {

  /**
   * Optional value that is present
   * @param get
   *   the value
   * @tparam A
   *   type of the value
   */
  final case class Present[+A](get: A) extends Optional[A] {
    override def isEmpty: Boolean   = false
    override def isDefined: Boolean = true
    override def nonEmpty: Boolean  = true
  }

  /**
   * Optional value that is absent
   */
  case object Absent extends Optional[Nothing] {
    override def isEmpty: Boolean   = true
    override def isDefined: Boolean = false
    override def nonEmpty: Boolean  = false
  }

  /**
   * Converts a plain value to `Present(value)` where an `Optional` is expected. A value of type `A` that is
   * `null` also becomes `Present(null)`; a `null` literal is not converted, because it is already an `Optional`.
   */
  implicit def AllValuesAreNullable[A](value: A): Optional[A] = Present(value)

  /**
   * Converts an `Option` to an `Optional` where an `Optional` is expected: `Some(a)` becomes
   * `Present(a)` and `None` becomes `Absent`.
   */
  implicit def OptionIsNullable[A](value: Option[A]): Optional[A] =
    value match {
      case Some(value) => Present(value)
      case None        => Absent
    }
}
