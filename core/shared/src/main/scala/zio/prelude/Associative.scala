/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

import zio.prelude.coherent.AssociativeEqual
import zio.prelude.newtypes._
import zio.prelude.newtypes._
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}
import zio.{Chunk, NonEmptyChunk}

import scala.annotation.tailrec

/**
 * The `Associative[A]` type class describes an associative binary operator
 * for a type `A`. For example, addition for integers, and string
 * concatenation for strings.
 *
 * `Associative` is at the top of the hierarchy for abstracting over operations
 * to combine types because while there are some operations that are not
 * associative but do obey other laws, it is generally difficult to combine
 * more than two values in interesting ways with these operators, and thus to
 * build solutions to more complicated problems out of solutions to simpler
 * ones.
 *
 * For example, the mean of two numbers is an operation that is commutative but
 * not associative. However, the lack of associativity is an indication that we
 * can't combine the means of multiple values in an interesting way with this
 * definition. If we attempt to take the mean of three values we always place
 * twice as much weight on one number as the others, which is rarely what we
 * want.
 *
 * If we instead define this operation using a `StatsCounter` object then means
 * can be combined in ways that are associative, commutative, and have an
 * identity element, supporting much more interesting modes of composition.
 */
trait Associative[A] {
  def combine(l: => A, r: => A): A

  final def repeat(a: A)(n: Int): A = {
    @tailrec
    def repeatHelper(res: A, n: Int): A =
      if (n <= 1) res
      else
        repeatHelper(combine(res, a), n - 1)
    repeatHelper(a, n)
  }

  def multiplyOption(n: Int)(a: A): Option[A] = {
    def multiplyHelper(res: A, n: Int): Option[A] =
      if (n <= 0) None
      else if (n == 1) Some(res)
      else multiplyHelper(combine(a, res), n - 1)
    multiplyHelper(a, n)
  }
}

object Associative extends AssociativeLowPriority with Lawful[AssociativeEqual] {

  /**
   * The associativity law states that for some binary operator `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 * a2) * a3 === a1 * (a2 * a3)
   * }}}
   */
  lazy val associativityLaw: Laws[AssociativeEqual] =
    new Laws.Law3[AssociativeEqual]("associativityLaw") {
      def apply[A: AssociativeEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 <> (a2 <> a3)) <-> ((a1 <> a2) <> a3)
    }

  /**
   * The set of all laws that instances of `Associative` must satisfy.
   */
  lazy val laws: Laws[AssociativeEqual] =
    associativityLaw

  /**
   * Summons an implicit `Associative[A]`.
   */
  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  /**
   * Constructs an `Associative` instance from an associative binary operator.
   */
  def make[A](f: (A, A) => A): Associative[A] =
    (l, r) => f(l, r)

  /**
   * The `Commutative`, `Idempotent` and `Inverse` instance for the conjunction of `Boolean`
   * values.
   */
  implicit val BooleanConjunctionIdempotentInverse: Commutative[And] with Idempotent[And] with Inverse[And] =
    new Commutative[And] with Idempotent[And] with Inverse[And] {
      def combine(l: => And, r: => And): And = And(l && r)
      val identity: And                      = And(true)
      def inverse(l: => And, r: => And): And = And(l || !r)
    }

  /**
   * The `Commutative`, `Idempotent` and `Inverse` instance for the disjunction of `Boolean`
   * values.
   */
  implicit val BooleanDisjunctionIdempotentInverse: Commutative[Or] with Idempotent[Or] with Inverse[Or] =
    new Commutative[Or] with Idempotent[Or] with Inverse[Or] {
      def combine(l: => Or, r: => Or): Or = Or(l || r)
      val identity: Or                    = Or(false)
      def inverse(l: => Or, r: => Or): Or = Or(l && !r)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Boolean` values.
   */
  implicit val BooleanMaxIdempotentIdentity
    : Commutative[Max[Boolean]] with Idempotent[Max[Boolean]] with Identity[Max[Boolean]] =
    new Commutative[Max[Boolean]] with Idempotent[Max[Boolean]] with Identity[Max[Boolean]] {
      def combine(l: => Max[Boolean], r: => Max[Boolean]): Max[Boolean] = Max(l || r)
      val identity: Max[Boolean]                                        = Max(false)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Boolean` values.
   */
  implicit val BooleanMinIdempotentIdentity
    : Commutative[Min[Boolean]] with Idempotent[Min[Boolean]] with Identity[Min[Boolean]] =
    new Commutative[Min[Boolean]] with Idempotent[Min[Boolean]] with Identity[Min[Boolean]] {
      def combine(l: => Min[Boolean], r: => Min[Boolean]): Min[Boolean] = Min(l && r)
      val identity: Min[Boolean]                                        = Min(true)
    }

  /**
   * The `Commutative`, `Idempotent` and `Inverse` instance for the product of `Boolean` values.
   */
  implicit val BooleanProdCommutativeIdentity
    : Commutative[Prod[Boolean]] with Idempotent[Prod[Boolean]] with Inverse[Prod[Boolean]] =
    new Commutative[Prod[Boolean]] with Idempotent[Prod[Boolean]] with Inverse[Prod[Boolean]] {
      def combine(l: => Prod[Boolean], r: => Prod[Boolean]): Prod[Boolean] = Prod(l && r)
      val identity: Prod[Boolean]                                          = Prod(true)
      def inverse(l: => Prod[Boolean], r: => Prod[Boolean]): Prod[Boolean] = Prod(l || !r)
    }

  /**
   * The `Commutative`, `Idempotent` and `Inverse` instance for the sum of `Boolean` values.
   */
  implicit val BooleanSumCommutativeInverse
    : Commutative[Sum[Boolean]] with Idempotent[Sum[Boolean]] with Inverse[Sum[Boolean]] =
    new Commutative[Sum[Boolean]] with Idempotent[Sum[Boolean]] with Inverse[Sum[Boolean]] {
      def combine(l: => Sum[Boolean], r: => Sum[Boolean]): Sum[Boolean] = Sum(l || r)
      val identity: Sum[Boolean]                                        = Sum(false)
      def inverse(l: => Sum[Boolean], r: => Sum[Boolean]): Sum[Boolean] = Sum(l && !r)
    }

  /**
   * The `Commutative`, `Idempotent` instance for the max of `BigDecimal` values
   */
  implicit val BigDecimalMaxCommutativeIdempotent: Commutative[Max[BigDecimal]] with Idempotent[Max[BigDecimal]] =
    new Commutative[Max[BigDecimal]] with Idempotent[Max[BigDecimal]] {
      override def combine(l: => Max[BigDecimal], r: => Max[BigDecimal]): Max[BigDecimal] = Max(l max r)
    }

  /**
   * The `Commutative`, `Idempotent` instance for the min of `BigDecimal` values
   */
  implicit val BigDecimalMinCommutativeIdempotent: Commutative[Min[BigDecimal]] with Idempotent[Min[BigDecimal]] =
    new Commutative[Min[BigDecimal]] with Idempotent[Min[BigDecimal]] {
      override def combine(l: => Min[BigDecimal], r: => Min[BigDecimal]): Min[BigDecimal] = Min(l min r)
    }

  /**
   * The `Commutative`, `Idempotent` instance for the product of `BigDecimal` values
   */
  implicit val BigDecimalProdCommutativeIdempotent: Commutative[Prod[BigDecimal]] with Idempotent[Prod[BigDecimal]] =
    new Commutative[Prod[BigDecimal]] with Idempotent[Prod[BigDecimal]] {
      override def combine(l: => Prod[BigDecimal], r: => Prod[BigDecimal]): Prod[BigDecimal] = Prod(l * r)
    }

  /**
   * The `Commutative`, `Idempotent` instance for the sum of `BigDecimal` values
   */
  implicit val BigDecimalSumCommutativeIdempotent: Commutative[Sum[BigDecimal]] with Idempotent[Sum[BigDecimal]] =
    new Commutative[Sum[BigDecimal]] with Idempotent[Sum[BigDecimal]] {
      override def combine(l: => Sum[BigDecimal], r: => Sum[BigDecimal]): Sum[BigDecimal] = Sum(l + r)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Byte` values.
   */
  implicit val ByteMaxIdempotentIdentity: Commutative[Max[Byte]] with Idempotent[Max[Byte]] with Identity[Max[Byte]] =
    new Commutative[Max[Byte]] with Idempotent[Max[Byte]] with Identity[Max[Byte]] {
      def combine(l: => Max[Byte], r: => Max[Byte]): Max[Byte] = Max(l max r)
      val identity: Max[Byte]                                  = Max(Byte.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Byte` values.
   */
  implicit val ByteMinIdempotentIdentity: Commutative[Min[Byte]] with Idempotent[Min[Byte]] with Identity[Min[Byte]] =
    new Commutative[Min[Byte]] with Idempotent[Min[Byte]] with Identity[Min[Byte]] {
      def combine(l: => Min[Byte], r: => Min[Byte]): Min[Byte] = Min(l min r)
      val identity: Min[Byte]                                  = Min(Byte.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Byte`
   * values.
   */
  implicit val ByteProdCommutativeIdentity: Commutative[Prod[Byte]] with Identity[Prod[Byte]] =
    new Commutative[Prod[Byte]] with Identity[Prod[Byte]] {
      def combine(l: => Prod[Byte], r: => Prod[Byte]): Prod[Byte] = Prod((l * r).toByte)
      val identity: Prod[Byte]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Byte` values.
   */
  implicit val ByteSumCommutativeInverse: Commutative[Sum[Byte]] with Inverse[Sum[Byte]] =
    new Commutative[Sum[Byte]] with Inverse[Sum[Byte]] {
      def combine(l: => Sum[Byte], r: => Sum[Byte]): Sum[Byte] = Sum((l + r).toByte)
      val identity: Sum[Byte]                                  = Sum(0)
      def inverse(l: => Sum[Byte], r: => Sum[Byte]): Sum[Byte] = Sum((l - r).toByte)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Char` values.
   */
  implicit val CharMaxIdempotentIdentity: Commutative[Max[Char]] with Idempotent[Max[Char]] with Identity[Max[Char]] =
    new Commutative[Max[Char]] with Idempotent[Max[Char]] with Identity[Max[Char]] {
      def combine(l: => Max[Char], r: => Max[Char]): Max[Char] = Max(l max r)
      val identity: Max[Char]                                  = Max(Char.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Char` values.
   */
  implicit val CharMinIdempotentIdentity: Commutative[Min[Char]] with Idempotent[Min[Char]] with Identity[Min[Char]] =
    new Commutative[Min[Char]] with Idempotent[Min[Char]] with Identity[Min[Char]] {
      def combine(l: => Min[Char], r: => Min[Char]): Min[Char] = Min(l min r)
      val identity: Min[Char]                                  = Min(Char.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Char`
   * values.
   */
  implicit val CharProdCommutativeIdentity: Commutative[Prod[Char]] with Identity[Prod[Char]] =
    new Commutative[Prod[Char]] with Identity[Prod[Char]] {
      def combine(l: => Prod[Char], r: => Prod[Char]): Prod[Char] = Prod((l * r).toChar)
      val identity: Prod[Char]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Char` values.
   */
  implicit val CharSumCommutativeInverse: Commutative[Sum[Char]] with Inverse[Sum[Char]] =
    new Commutative[Sum[Char]] with Inverse[Sum[Char]] {
      def combine(l: => Sum[Char], r: => Sum[Char]): Sum[Char] = Sum((l + r).toChar)
      val identity: Sum[Char]                                  = Sum(0)
      def inverse(l: => Sum[Char], r: => Sum[Char]): Sum[Char] = Sum((l - r).toChar)
    }

  /**
   * The `Identity` instance for the concatenation of `Chunk[A]` values.
   */
  implicit def ChunkIdentity[A]: Identity[Chunk[A]] =
    Identity.make(Chunk.empty, _ ++ _)

  /**
   * Derives an `Associative[F[A]]` given a `Derive[F, Associative]` and an
   * `Associative[A]`.
   */
  implicit def DeriveAssociative[F[_], A](implicit
    derive: Derive[F, Associative],
    associative: Associative[A]
  ): Associative[F[A]] =
    derive.derive(associative)

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Double` values.
   */
  implicit val DoubleMaxCommutativeIdempotentIdentity
    : Commutative[Max[Double]] with Idempotent[Max[Double]] with Identity[Max[Double]] =
    new Commutative[Max[Double]] with Idempotent[Max[Double]] with Identity[Max[Double]] {
      def combine(l: => Max[Double], r: => Max[Double]): Max[Double] = Max(l max r)
      val identity: Max[Double]                                      = Max(Double.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Double` values.
   */
  implicit val DoubleMinCommutativeIdempotentIdentity
    : Commutative[Min[Double]] with Idempotent[Min[Double]] with Identity[Min[Double]] =
    new Commutative[Min[Double]] with Idempotent[Min[Double]] with Identity[Min[Double]] {
      def combine(l: => Min[Double], r: => Min[Double]): Min[Double] = Min(l min r)
      val identity: Min[Double]                                      = Min(Double.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Double`
   * values.
   */
  implicit val DoubleProdCommutativeIdentity: Commutative[Prod[Double]] with Identity[Prod[Double]] =
    new Commutative[Prod[Double]] with Identity[Prod[Double]] {
      def combine(l: => Prod[Double], r: => Prod[Double]): Prod[Double] = Prod(l * r)
      val identity: Prod[Double]                                        = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Double` values.
   */
  implicit val DoubleSumCommutativeInverse: Commutative[Sum[Double]] with Inverse[Sum[Double]] =
    new Commutative[Sum[Double]] with Inverse[Sum[Double]] {
      def combine(l: => Sum[Double], r: => Sum[Double]): Sum[Double] = Sum(l + r)
      val identity: Sum[Double]                                      = Sum(0)
      def inverse(l: => Sum[Double], r: => Sum[Double]): Sum[Double] = Sum(l - r)
    }

  /**
   * Derives an `Associative[Either[E, A]]` given an `Associative[A]`.
   */
  implicit def EitherAssociative[E, A: Associative]: Associative[Either[E, A]] =
    make {
      case (Left(l), _)         => Left(l)
      case (_, Left(r))         => Left(r)
      case (Right(l), Right(r)) => Right(l <> r)
    }

  /**
   * The `Associative` instance for the first of `A` values.
   */
  implicit def FirstAssociative[A]: Associative[First[A]] =
    make((l: First[A], _: First[A]) => l)

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Float` values.
   */
  implicit val FloatMaxCommutativeIdempotentIdentity
    : Commutative[Max[Float]] with Idempotent[Max[Float]] with Identity[Max[Float]] =
    new Commutative[Max[Float]] with Idempotent[Max[Float]] with Identity[Max[Float]] {
      def combine(l: => Max[Float], r: => Max[Float]): Max[Float] = Max(l max r)
      val identity: Max[Float]                                    = Max(Float.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Float` values.
   */
  implicit val FloatMinCommutativeIdempotentIdentity
    : Commutative[Min[Float]] with Idempotent[Min[Float]] with Identity[Min[Float]] =
    new Commutative[Min[Float]] with Idempotent[Min[Float]] with Identity[Min[Float]] {
      def combine(l: => Min[Float], r: => Min[Float]): Min[Float] = Min(l min r)
      val identity: Min[Float]                                    = Min(Float.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Float`
   * values.
   */
  implicit val FloatProdCommutativeIdentity: Commutative[Prod[Float]] with Identity[Prod[Float]] =
    new Commutative[Prod[Float]] with Identity[Prod[Float]] {
      def combine(l: => Prod[Float], r: => Prod[Float]): Prod[Float] = Prod(l * r)
      val identity: Prod[Float]                                      = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Float` values.
   */
  implicit val FloatSumCommutativeInverse: Commutative[Sum[Float]] with Inverse[Sum[Float]] =
    new Commutative[Sum[Float]] with Inverse[Sum[Float]] {
      def combine(l: => Sum[Float], r: => Sum[Float]): Sum[Float] = Sum(l + r)
      val identity: Sum[Float]                                    = Sum(0)
      def inverse(l: => Sum[Float], r: => Sum[Float]): Sum[Float] = Sum(l - r)
    }

  implicit def Function1Identity[A]: Identity[A => A] = new Identity[A => A] {
    override def identity: A => A = scala.Predef.identity

    override def combine(l: => A => A, r: => A => A): A => A = l.andThen(r)
  }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Int` values.
   */
  implicit val IntMaxIdempotentIdentity: Commutative[Max[Int]] with Idempotent[Max[Int]] with Identity[Max[Int]] =
    new Commutative[Max[Int]] with Idempotent[Max[Int]] with Identity[Max[Int]] {
      def combine(l: => Max[Int], r: => Max[Int]): Max[Int] = Max(l max r)
      val identity: Max[Int]                                = Max(Int.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Int` values.
   */
  implicit val IntMinIdempotentIdentity: Commutative[Min[Int]] with Idempotent[Min[Int]] with Identity[Min[Int]] =
    new Commutative[Min[Int]] with Idempotent[Min[Int]] with Identity[Min[Int]] {
      def combine(l: => Min[Int], r: => Min[Int]): Min[Int] = Min(l min r)
      val identity: Min[Int]                                = Min(Int.MaxValue)
    }

  /**
   * The `Associative` instance for the last of `A` values.
   */
  implicit def LastAssociative[A]: Associative[Last[A]] =
    make((_: Last[A], r: Last[A]) => r)

  /**
   * The `Identity` instance for the concatenation of `List[A]` values.
   */
  implicit def ListIdentity[A]: Identity[List[A]] =
    Identity.make[List[A]](Nil, _ ++ _)

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Long` values.
   */
  implicit val LongMaxIdempotentIdentity: Commutative[Max[Long]] with Idempotent[Max[Long]] with Identity[Max[Long]] =
    new Commutative[Max[Long]] with Idempotent[Max[Long]] with Identity[Max[Long]] {
      def combine(l: => Max[Long], r: => Max[Long]): Max[Long] = Max(l max r)
      val identity: Max[Long]                                  = Max(Long.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Long` values.
   */
  implicit val LongMinIdempotentIdentity: Commutative[Min[Long]] with Idempotent[Min[Long]] with Identity[Min[Long]] =
    new Commutative[Min[Long]] with Idempotent[Min[Long]] with Identity[Min[Long]] {
      def combine(l: => Min[Long], r: => Min[Long]): Min[Long] = Min(l min r)
      val identity: Min[Long]                                  = Min(Long.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Long`
   * values.
   */
  implicit val LongProdCommutativeIdentity: Commutative[Prod[Long]] with Identity[Prod[Long]] =
    new Commutative[Prod[Long]] with Identity[Prod[Long]] {
      def combine(l: => Prod[Long], r: => Prod[Long]): Prod[Long] = Prod(l * r)
      val identity: Prod[Long]                                    = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Long` values.
   */
  implicit val LongSumCommutativeInverse: Commutative[Sum[Long]] with Inverse[Sum[Long]] =
    new Commutative[Sum[Long]] with Inverse[Sum[Long]] {
      def combine(l: => Sum[Long], r: => Sum[Long]): Sum[Long] = Sum(l + r)
      val identity: Sum[Long]                                  = Sum(0)
      def inverse(l: => Sum[Long], r: => Sum[Long]): Sum[Long] = Sum(l - r)
    }

  /**
   * Derives an `Identity[Map[K, V]]` given an `Associative[V]`.
   */
  implicit def MapIdentity[K, V: Associative]: Identity[Map[K, V]] =
    new Identity[Map[K, V]] {
      def identity: Map[K, V] = Map()

      def combine(l: => Map[K, V], r: => Map[K, V]): Map[K, V] =
        r.foldLeft(l) { case (map, (k, v)) =>
          map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  /**
   * The `Commutative` instance for the maximum of `A` values for which an
   * `Ord` is defined.
   */
  implicit def MaxCommutative[A: Ord]: Commutative[Max[A]] =
    Commutative.make((l: Max[A], r: Max[A]) => if (l >= r) l else r)

  /**
   * The `Commutative` instance for the minimum of `A` values for which an
   * `Ord` is defined.
   */
  implicit def MinCommutative[A: Ord]: Commutative[Min[A]] =
    Commutative.make((l: Min[A], r: Min[A]) => if (l <= r) l else r)

  /**
   * The `Commutative` and `Identity` instance for the product of `Natural` values.
   */
  implicit val NaturalProdCommutativeIdentity: Commutative[Prod[Natural]] with Identity[Prod[Natural]] =
    new Commutative[Prod[Natural]] with Identity[Prod[Natural]] {
      def combine(l: => Prod[Natural], r: => Prod[Natural]): Prod[Natural] = Prod(Natural.times(l, r))
      val identity: Prod[Natural]                                          = Prod(Natural.one)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Narutal` values.
   */
  implicit val NaturalSumCommutativeInverse: Commutative[Sum[Natural]] with Inverse[Sum[Natural]] =
    new Commutative[Sum[Natural]] with Inverse[Sum[Natural]] {
      def combine(l: => Sum[Natural], r: => Sum[Natural]): Sum[Natural] = Sum(Natural.plus(l, r))
      val identity: Sum[Natural]                                        = Sum(Natural.zero)
      def inverse(l: => Sum[Natural], r: => Sum[Natural]): Sum[Natural] = Sum(Natural.minus(l, r))
    }

  /**
   * The `Associative` instance for the concatenation of `NonEmptyChunk[A]`
   * values.
   */
  implicit def NonEmptyChunkAssociative[A]: Associative[NonEmptyChunk[A]] =
    make(_ ++ _)

  /**
   * Derives an `Identity[Option[A]]` given an `Associative[A]`.
   */
  implicit def OptionIdentity[A: Associative]: Identity[Option[A]] =
    Identity.make(
      None,
      {
        case (Some(l), Some(r)) => Some(l <> r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case _                  => None
      }
    )

  /**
   * The `Commutative`, `Idempotent` and `Inverse` instance for the union of `Set[A]`
   * values.
   */
  implicit def SetIdempotentInverse[A]: Commutative[Set[A]] with Idempotent[Set[A]] with Inverse[Set[A]] =
    new Commutative[Set[A]] with Idempotent[Set[A]] with Inverse[Set[A]] {
      def combine(l: => Set[A], r: => Set[A]): Set[A] = l | r
      val identity: Set[A]                            = Set.empty
      def inverse(l: => Set[A], r: => Set[A]): Set[A] = l &~ r
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the max of `Short` values.
   */
  implicit val ShortMaxIdempotentIdentity
    : Commutative[Max[Short]] with Idempotent[Max[Short]] with Identity[Max[Short]] =
    new Commutative[Max[Short]] with Idempotent[Max[Short]] with Identity[Max[Short]] {
      def combine(l: => Max[Short], r: => Max[Short]): Max[Short] = Max(l max r)
      val identity: Max[Short]                                    = Max(Short.MinValue)
    }

  /**
   * The `Commutative`, `Idempotent` and `Identity` instance for the min of `Short` values.
   */
  implicit val ShortMinIdempotentIdentity
    : Commutative[Min[Short]] with Idempotent[Min[Short]] with Identity[Min[Short]] =
    new Commutative[Min[Short]] with Idempotent[Min[Short]] with Identity[Min[Short]] {
      def combine(l: => Min[Short], r: => Min[Short]): Min[Short] = Min(l min r)
      val identity: Min[Short]                                    = Min(Short.MaxValue)
    }

  /**
   * The `Commutative` and `Identity` instance for the product of `Short`
   * values.
   */
  implicit val ShortProdCommutativeIdentity: Commutative[Prod[Short]] with Identity[Prod[Short]] =
    new Commutative[Prod[Short]] with Identity[Prod[Short]] {
      def combine(l: => Prod[Short], r: => Prod[Short]): Prod[Short] = Prod((l * r).toShort)
      val identity: Prod[Short]                                      = Prod(1)
    }

  /**
   * The `Commutative` and `Identity` instance for the sum of `Short` values.
   */
  implicit val ShortSumCommutativeIdentity: Commutative[Sum[Short]] with Inverse[Sum[Short]] =
    new Commutative[Sum[Short]] with Inverse[Sum[Short]] {
      def combine(l: => Sum[Short], r: => Sum[Short]): Sum[Short] = Sum((l + r).toShort)
      val identity: Sum[Short]                                    = Sum(0)
      def inverse(l: => Sum[Short], r: => Sum[Short]): Sum[Short] = Sum((l - r).toShort)
    }

  /**
   * The `Identity` instance for the concatenation of `String` values.
   */
  implicit val StringIdentity: Identity[String] =
    Identity.make("", _ + _)

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple2Associative[A: Associative, B: Associative]: Associative[(A, B)] =
    make { case ((a1, b1), (a2, b2)) =>
      (a1 <> a2, b1 <> b2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple3Associative[A: Associative, B: Associative, C: Associative]: Associative[(A, B, C)] =
    make { case ((a1, b1, c1), (a2, b2, c2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple4Associative[A: Associative, B: Associative, C: Associative, D: Associative]
    : Associative[(A, B, C, D)] =
    make { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple5Associative[A: Associative, B: Associative, C: Associative, D: Associative, E: Associative]
    : Associative[(A, B, C, D, E)] =
    make { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
      (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple6Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative
  ]: Associative[(A, B, C, D, E, F)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1),
            (a2, b2, c2, d2, e2, f2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple7Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative
  ]: Associative[(A, B, C, D, E, F, G)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1),
            (a2, b2, c2, d2, e2, f2, g2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple8Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative
  ]: Associative[(A, B, C, D, E, F, G, H)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1),
            (a2, b2, c2, d2, e2, f2, g2, h2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple9Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2)
          ) =>
        (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2, g1 <> g2, h1 <> h2, i1 <> i2)
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple10Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple11Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple12Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple13Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple14Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple15Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple16Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple17Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple18Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple19Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple20Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple21Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative,
    U: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2,
          u1 <> u2
        )
    }

  /**
   * Derives an `Associative` for a product type given an `Associative` for
   * each element of the product type.
   */
  implicit def Tuple22Associative[
    A: Associative,
    B: Associative,
    C: Associative,
    D: Associative,
    E: Associative,
    F: Associative,
    G: Associative,
    H: Associative,
    I: Associative,
    J: Associative,
    K: Associative,
    L: Associative,
    M: Associative,
    N: Associative,
    O: Associative,
    P: Associative,
    Q: Associative,
    R: Associative,
    S: Associative,
    T: Associative,
    U: Associative,
    V: Associative
  ]: Associative[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    make {
      case (
            (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
            (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
          ) =>
        (
          a1 <> a2,
          b1 <> b2,
          c1 <> c2,
          d1 <> d2,
          e1 <> e2,
          f1 <> f2,
          g1 <> g2,
          h1 <> h2,
          i1 <> i2,
          j1 <> j2,
          k1 <> k2,
          l1 <> l2,
          m1 <> m2,
          n1 <> n2,
          o1 <> o2,
          p1 <> p2,
          q1 <> q2,
          r1 <> r2,
          s1 <> s2,
          t1 <> t2,
          u1 <> u2,
          v1 <> v2
        )
    }

  /**
   * The `Identity` instance for the concatenation of `Vector[A]` values.
   */
  implicit def VectorIdentity[A]: Identity[Vector[A]] =
    Identity.make(Vector.empty, _ ++ _)
}

trait AssociativeLowPriority {

  /**
   * The `Commutative` and `Identity` instance for the product of `Int` values.
   */
  implicit val IntProdCommutativeIdentity: Commutative[Prod[Int]] with Identity[Prod[Int]] =
    new Commutative[Prod[Int]] with Identity[Prod[Int]] {
      def combine(l: => Prod[Int], r: => Prod[Int]): Prod[Int] = Prod(l * r)
      val identity: Prod[Int]                                  = Prod(1)
    }

  /**
   * The `Commutative` and `Inverse` instance for the sum of `Int` values.
   */
  implicit val IntSumCommutativeInverse: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
    new Commutative[Sum[Int]] with Inverse[Sum[Int]] {
      def combine(l: => Sum[Int], r: => Sum[Int]): Sum[Int] = Sum(l + r)
      val identity: Sum[Int]                                = Sum(0)
      def inverse(l: => Sum[Int], r: => Sum[Int]): Sum[Int] = Sum(l - r)
    }
}

trait AssociativeSyntax {

  /**
   * Provides infix syntax for combining two values with an associative
   * operation.
   */
  implicit class AssociativeOps[A](l: A) {

    /**
     * A symbolic alias for `combine`.
     */
    def <>[A1 >: A](r: => A1)(implicit associative: Associative[A1]): A1 =
      associative.combine(l, r)

    /**
     * Associatively combines this value with the specified value
     */
    def combine[A1 >: A](r: => A1)(implicit associative: Associative[A1]): A1 =
      associative.combine(l, r)

    /**
     * Associatively repeats value 'n' times
     */
    def repeat(n: Int)(implicit associative: Associative[A]): A =
      associative.repeat(l)(n)

    /**
     * Associatively multiplies value 'n' times
     */
    def multiplyOption(n: Int)(implicit associative: Associative[A]): Option[A] =
      associative.multiplyOption(n)(l)
  }

}
