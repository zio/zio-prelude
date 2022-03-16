package zio.prelude.data

import zio.Chunk

import scala.language.implicitConversions

/**
 * Custom option type to be used for wrapping external data models where most of the fields are
 * defined to be optional.
 *
 * Instances of Optional are either [[Optional.Present]] or [[Optional.Absent]].
 *
 * The only difference between this type and [[scala.Option]] is that there is an implicit
 * conversion defined from `A`` to `Optional[A]`, and from `Option[A]`` to `Optional[A]`.
 */
sealed trait Optional[+A] { self =>
  val isEmpty: Boolean
  val isDefined: Boolean
  val nonEmpty: Boolean

  /**
   * Converts this optional value to standard [[scala.Option]]
   */
  final def toOption: Option[A] = self match {
    case Optional.Present(get) => Some(get)
    case Optional.Absent       => None
  }

  final def getOrElse[A0 >: A](default: => A0): A0 =
    self match {
      case Optional.Present(get) => get
      case Optional.Absent       => default
    }

  final def map[B](f: A => B): Optional[B] =
    self match {
      case Optional.Present(get) => Optional.Present(f(get))
      case Optional.Absent       => Optional.Absent
    }

  final def flatMap[B](f: A => Optional[B]): Optional[B] =
    self match {
      case Optional.Present(get) => f(get)
      case Optional.Absent       => Optional.Absent
    }

  final def fold[B](ifEmpty: => B)(f: A => B): B =
    self match {
      case Optional.Present(get) => f(get)
      case Optional.Absent       => ifEmpty
    }

  final def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    self match {
      case Optional.Present(get) => ev(get)
      case Optional.Absent       => None
    }

  final def toLeft[R](right: R): Either[A, R] =
    self match {
      case Optional.Present(get) => Left(get)
      case Optional.Absent       => Right(right)
    }

  final def toRight[L](left: L): Either[L, A] =
    self match {
      case Optional.Present(get) => Right(get)
      case Optional.Absent       => Left(left)
    }

  final def filter(p: A => Boolean): Optional[A] =
    self match {
      case Optional.Present(get) => if (p(get)) this else Optional.Absent
      case Optional.Absent       => Optional.Absent
    }

  final def filterNot(p: A => Boolean): Optional[A] =
    self match {
      case Optional.Present(get) => if (!p(get)) this else Optional.Absent
      case Optional.Absent       => Optional.Absent
    }

  final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Optional[B]               = self filter p map f
    def flatMap[B](f: A => Optional[B]): Optional[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit                  = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter      = new WithFilter(x => p(x) && q(x))
  }

  final def contains[A1 >: A](elem: A1): Boolean =
    self match {
      case Optional.Present(get) => get == elem
      case Optional.Absent       => false
    }

  final def exists(p: A => Boolean): Boolean =
    self match {
      case Optional.Present(get) => p(get)
      case Optional.Absent       => false
    }

  final def forall(p: A => Boolean): Boolean =
    self match {
      case Optional.Present(get) => p(get)
      case Optional.Absent       => true
    }

  final def foreach[U](f: A => U): Unit =
    self match {
      case Optional.Present(get) => val _ = f(get)
      case Optional.Absent       =>
    }

  final def collect[B](pf: PartialFunction[A, B]): Optional[B] =
    self match {
      case Optional.Present(get) => pf.lift(get)
      case Optional.Absent       => Optional.Absent
    }

  final def orElse[B >: A](other: Optional[B]): Optional[B] =
    self match {
      case Optional.Present(_) => self
      case Optional.Absent     => other
    }

  final def iterator: Iterator[A] =
    self match {
      case Optional.Present(get) => Iterator.single(get)
      case Optional.Absent       => Iterator.empty
    }

  final def toChunk: Chunk[A] =
    self match {
      case Optional.Present(get) => Chunk.single(get)
      case Optional.Absent       => Chunk.empty
    }

  final def toList: List[A] =
    self match {
      case Optional.Present(get) => get :: Nil
      case Optional.Absent       => Nil
    }

  final def toVector: Vector[A] =
    self match {
      case Optional.Present(get) => Vector(get)
      case Optional.Absent       => Vector.empty
    }
}

object Optional {

  /**
   * Optional value that is present
   * @param get
   *   the value
   * @tparam A
   *   type of the value
   */
  final case class Present[+A](get: A) extends Optional[A] {
    override val isEmpty: Boolean   = false
    override val isDefined: Boolean = true
    override val nonEmpty: Boolean  = true
  }

  /**
   * Optional value that is absent
   */
  case object Absent extends Optional[Nothing] {
    override val isEmpty: Boolean   = true
    override val isDefined: Boolean = false
    override val nonEmpty: Boolean  = false
  }

  implicit def AllValuesAreNullable[A](value: A): Optional[A]     = Present(value)
  implicit def OptionIsNullable[A](value: Option[A]): Optional[A] =
    value match {
      case Some(value) => Present(value)
      case None        => Absent
    }
}
