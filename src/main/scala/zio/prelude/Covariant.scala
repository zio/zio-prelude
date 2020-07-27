package zio.prelude

import zio._
import zio.prelude.coherent.CovariantEqualF
import zio.prelude.newtypes.{ Failure, FailureIn, FailureOut }
import zio.stream.ZStream
import zio.test.TestResult
import zio.test.laws._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

/**
 * `Covariant[F]` provides implicit evidence that `F[+_]` is a covariant
 * endofunctor in the category of Scala objects.
 *
 * Covariant instances of type `F[A]` "produce" values of type `A` in some
 * sense. In some cases, such as with a `List[A]`, this means that they
 * contain values of type `A`, in which case we can simply access the elements
 * of the collection. In other cases it means that output values of type `A`
 * which may not already exists, such as with a `Function0[A]` that produces
 * `A` values when invoked.
 *
 * Common examples of covariant instances in ZIO includes effects effects with
 * respect to their error and value types, sinks with respect to their error
 * and output types, and queues and references with respect to their error and
 * output types.
 *
 * `Covariant` instances support a `map` operation which allows transforming
 * the output type given a function from the old output type to the new output
 * type. For example, if we have a `List[String]` and a function
 * `String => Int` that returns the length of a string, then we can construct
 * a `List[Int]` with the length of each string.
 */
trait Covariant[F[+_]] extends Invariant[F] {

  /**
   * Lift a function from `A` to `B` to a function from `F[A]` to `F[B]`.
   */
  def map[A, B](f: A => B): F[A] => F[B]

  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => map(f.to)(fa), (fb: F[B]) => map(f.from)(fb))
}

object Covariant extends LawfulF.Covariant[CovariantEqualF, Equal] {

  /**
   * Mapping with the identity function must be an identity function.
   */
  val identityLaw: LawsF.Covariant[CovariantEqualF, Equal] =
    new LawsF.Covariant.Law1[CovariantEqualF, Equal]("identityLaw") {
      def apply[F[+_]: CovariantEqualF, A: Equal](fa: F[A]): TestResult =
        fa.map(identity) <-> fa
    }

  /**
   * Mapping by `f` followed by `g` must be the same as mapping with the
   * composition of `f` and `g`.
   */
  val compositionLaw: LawsF.Covariant[CovariantEqualF, Equal] =
    new LawsF.Covariant.ComposeLaw[CovariantEqualF, Equal]("compositionLaw") {
      def apply[F[+_]: CovariantEqualF, A: Equal, B: Equal, C: Equal](fa: F[A], f: A => B, g: B => C): TestResult =
        fa.map(f).map(g) <-> fa.map(f andThen g)
    }

  /**
   * The set of all laws that instances of `Covariant` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantEqualF, Equal] =
    identityLaw + compositionLaw

  /**
   * Summons an implicit `Covariant[F]`.
   */
  def apply[F[+_]](implicit covariant: Covariant[F]): Covariant[F] =
    covariant

  /**
   * The `Covariant` instance for `Chunk`
   */
  implicit val ChunkCovariant: Covariant[Chunk] =
    new Covariant[Chunk] {
      def map[A, B](f: A => B): Chunk[A] => Chunk[B] = { chunk =>
        chunk.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Option`.
   */
  implicit val OptionCovariant: Covariant[Option] =
    new Covariant[Option] {
      def map[A, B](f: A => B): Option[A] => Option[B] = { option =>
        option.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Id`.
   */
  implicit val IdCovariant: Covariant[Id] =
    new Covariant[Id] {
      def map[A, B](f: A => B): Id[A] => Id[B] = { id =>
        Id(f(Id.unwrap(id)))
      }
    }

  /**
   * The `Covariant` instance for `List`
   */
  implicit val ListCovariant: Covariant[List] =
    new Covariant[List] {
      def map[A, B](f: A => B): List[A] => List[B] = { list =>
        list.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Vector`
   */
  implicit val VectorCovariant: Covariant[Vector] =
    new Covariant[Vector] {
      def map[A, B](f: A => B): Vector[A] => Vector[B] = { vector =>
        vector.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Map`
   */
  implicit def MapCovariant[K]: Covariant[({ type lambda[+v] = Map[K, v] })#lambda] =
    new Covariant[({ type lambda[+v] = Map[K, v] })#lambda] {
      override def map[A, B](f: A => B): Map[K, A] => Map[K, B] = { map =>
        map.map(entry => (entry._1, f(entry._2)))
      }
    }

  /**
   * The `Covariant` instance for `Either`
   */
  implicit def EitherCovariant[L]: Covariant[({ type lambda[+r] = Either[L, r] })#lambda] =
    new Covariant[({ type lambda[+r] = Either[L, r] })#lambda] {
      override def map[A, B](f: A => B): Either[L, A] => Either[L, B] = { either =>
        either.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `Either`
   */
  implicit def EitherFailureCovariant[R]: Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new Covariant[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      override def map[L, L1](f: L => L1): Failure[Either[L, R]] => Failure[Either[L1, R]] = { either =>
        Failure.wrap {
          Failure.unwrap(either) match {
            case Left(l)  => Left[L1, R](f(l))
            case Right(r) => Right[L1, R](r)
          }
        }
      }
    }

  /**
   * The `Covariant` instance for `Try`
   */
  implicit val TryCovariant: Covariant[Try] =
    new Covariant[Try] {
      def map[A, B](f: A => B): Try[A] => Try[B] = { tryA =>
        tryA.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Future`
   */
  implicit def FutureCovariant(implicit ec: ExecutionContext): Covariant[Future] =
    new Covariant[Future] {
      def map[A, B](f: A => B): Future[A] => Future[B] = { future =>
        future.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Function1`
   */
  implicit def Function1Covariant[T]: Covariant[({ type lambda[+x] = T => x })#lambda] =
    new Covariant[({ type lambda[+x] = T => x })#lambda] {
      override def map[A, B](f: A => B): (T => A) => T => B =
        function => t => f(function(t))
    }

  /**
   * The `Covariant` instance for `Function2`
   */
  implicit def Function2Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2) => A) => ((T1, T2) => B) =
        function => (t1, t2) => f(function(t1, t2))
    }

  /**
   * The `Covariant` instance for `Function3`
   */
  implicit def Function3Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3) => A) => ((T1, T2, T3) => B) =
        function => (t1, t2, t3) => f(function(t1, t2, t3))
    }

  /**
   * The `Covariant` instance for `Function4`
   */
  implicit def Function4Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4) => A) => ((T1, T2, T3, T4) => B) =
        function => (t1, t2, t3, t4) => f(function(t1, t2, t3, t4))
    }

  /**
   * The `Covariant` instance for `Function5`
   */
  implicit def Function5Covariant[T1, T2, T3, T4, T5]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5) => A) => ((T1, T2, T3, T4, T5) => B) =
        function => (t1, t2, t3, t4, t5) => f(function(t1, t2, t3, t4, t5))
    }

  /**
   * The `Covariant` instance for `Function6`
   */
  implicit def Function6Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6) => A) => ((T1, T2, T3, T4, T5, T6) => B) =
        function => (t1, t2, t3, t4, t5, t6) => f(function(t1, t2, t3, t4, t5, t6))
    }

  /**
   * The `Covariant` instance for `Function7`
   */
  implicit def Function7Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7) => x })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, T7) => A) => ((T1, T2, T3, T4, T5, T6, T7) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7) => f(function(t1, t2, t3, t4, t5, t6, t7))
    }

  /**
   * The `Covariant` instance for `Function8`
   */
  implicit def Function8Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8) => f(function(t1, t2, t3, t4, t5, t6, t7, t8))
    }

  /**
   * The `Covariant` instance for `Function9`
   */
  implicit def Function9Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9))
    }

  /**
   * The `Covariant` instance for `Function10`
   */
  implicit def Function10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => B) =
        function => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
    }

  /**
   * The `Covariant` instance for `Function11`
   */
  implicit def Function11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => A) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
    }

  /**
   * The `Covariant` instance for `Function12`
   */
  implicit def Function12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
    }

  /**
   * The `Covariant` instance for `Function13`
   */
  implicit def Function13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
    }

  /**
   * The `Covariant` instance for `Function14`
   */
  implicit def Function14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => x })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
    }

  /**
   * The `Covariant` instance for `Function15`
   */
  implicit def Function15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
    }

  /**
   * The `Covariant` instance for `Function16`
   */
  implicit def Function16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
    }

  /**
   * The `Covariant` instance for `Function17`
   */
  implicit def Function17Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
    }

  /**
   * The `Covariant` instance for `Function18`
   */
  implicit def Function18Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => x })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
    }

  /**
   * The `Covariant` instance for `Function19`
   */
  implicit def Function19Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => x })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => x })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
    }

  /**
   * The `Covariant` instance for `Function20`
   */
  implicit def Function20Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ]: Covariant[
    ({
      type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => x
      })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
    }

  /**
   * The `Covariant` instance for `Function21`
   */
  implicit def Function21Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => x
      })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => A
      ) => ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => B) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) =>
            f(function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
    }

  /**
   * The `Covariant` instance for `Function22`
   */
  implicit def Function22Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => x
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => x
      })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => A
      ) => (
        (
          T1,
          T2,
          T3,
          T4,
          T5,
          T6,
          T7,
          T8,
          T9,
          T10,
          T11,
          T12,
          T13,
          T14,
          T15,
          T16,
          T17,
          T18,
          T19,
          T20,
          T21,
          T22
        ) => B
      ) =
        function =>
          (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) =>
            f(
              function(
                t1,
                t2,
                t3,
                t4,
                t5,
                t6,
                t7,
                t8,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20,
                t21,
                t22
              )
            )
    }

  /**
   * Covariant instance for `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkCovariant: Covariant[NonEmptyChunk] = new Covariant[NonEmptyChunk] {
    override def map[A, B](f: A => B): NonEmptyChunk[A] => NonEmptyChunk[B] = { chunk =>
      chunk.map(f)
    }
  }

  /**
   * The `Covariant` instance for `Tuple2`
   */
  implicit def Tuple2Covariant[T1]: Covariant[({ type lambda[+x] = (T1, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, A)) => (T1, B) = { tuple =>
        (tuple._1, f(tuple._2))
      }
    }

  /**
   * The `Covariant` instance for `Tuple3`
   */
  implicit def Tuple3Covariant[T1, T2]: Covariant[({ type lambda[+x] = (T1, T2, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, A)) => (T1, T2, B) = { tuple =>
        (tuple._1, tuple._2, f(tuple._3))
      }
    }

  /**
   * The `Covariant` instance for `Tuple4`
   */
  implicit def Tuple4Covariant[T1, T2, T3]: Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, A)) => (T1, T2, T3, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, f(tuple._4))
      }
    }

  /**
   * The `Covariant` instance for `Tuple5`
   */
  implicit def Tuple5Covariant[T1, T2, T3, T4]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, A)) => (T1, T2, T3, T4, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, f(tuple._5))
      }
    }

  /**
   * The `Covariant` instance for `Tuple6`
   */
  implicit def Tuple6Covariant[T1, T2, T3, T4, T5]: Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, A)) => (T1, T2, T3, T4, T5, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, f(tuple._6))
      }
    }

  /**
   * The `Covariant` instance for `Tuple7`
   */
  implicit def Tuple7Covariant[T1, T2, T3, T4, T5, T6]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, A)) => (T1, T2, T3, T4, T5, T6, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, f(tuple._7))
      }
    }

  /**
   * The `Covariant` instance for `Tuple8`
   */
  implicit def Tuple8Covariant[T1, T2, T3, T4, T5, T6, T7]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, x) })#lambda] {
      override def map[A, B](f: A => B): ((T1, T2, T3, T4, T5, T6, T7, A)) => (T1, T2, T3, T4, T5, T6, T7, B) = {
        tuple =>
          (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, f(tuple._8))
      }
    }

  /**
   * The `Covariant` instance for `Tuple9`
   */
  implicit def Tuple9Covariant[T1, T2, T3, T4, T5, T6, T7, T8]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, f(tuple._9))
      }
    }

  /**
   * The `Covariant` instance for `Tuple10`
   */
  implicit def Tuple10Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, B) = { tuple =>
        (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, f(tuple._10))
      }
    }

  /**
   * The `Covariant` instance for `Tuple11`
   */
  implicit def Tuple11Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          f(tuple._11)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple12`
   */
  implicit def Tuple12Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A)) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, B) = {
        tuple =>
          (
            tuple._1,
            tuple._2,
            tuple._3,
            tuple._4,
            tuple._5,
            tuple._6,
            tuple._7,
            tuple._8,
            tuple._9,
            tuple._10,
            tuple._11,
            f(tuple._12)
          )
      }
    }

  /**
   * The `Covariant` instance for `Tuple13`
   */
  implicit def Tuple13Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          f(tuple._13)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple14`
   */
  implicit def Tuple14Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          f(tuple._14)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple15`
   */
  implicit def Tuple15Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          f(tuple._15)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple16`
   */
  implicit def Tuple16Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    : Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda] =
    new Covariant[({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, x) })#lambda] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          f(tuple._16)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple17`
   */
  implicit def Tuple17Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, x) })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, x) })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          f(tuple._17)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple18`
   */
  implicit def Tuple18Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: Covariant[
    ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, x) })#lambda
  ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, x) })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          f(tuple._18)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple19`
   */
  implicit def Tuple19Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, x) })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, x) })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          f(tuple._19)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple20`
   */
  implicit def Tuple20Covariant[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    : Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, x) })#lambda
    ] =
    new Covariant[
      ({ type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, x) })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          tuple._19,
          f(tuple._20)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple21`
   */
  implicit def Tuple21Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ]: Covariant[
    ({
      type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, x)
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, x)
      })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, B) = { tuple =>
        (
          tuple._1,
          tuple._2,
          tuple._3,
          tuple._4,
          tuple._5,
          tuple._6,
          tuple._7,
          tuple._8,
          tuple._9,
          tuple._10,
          tuple._11,
          tuple._12,
          tuple._13,
          tuple._14,
          tuple._15,
          tuple._16,
          tuple._17,
          tuple._18,
          tuple._19,
          tuple._20,
          f(tuple._21)
        )
      }
    }

  /**
   * The `Covariant` instance for `Tuple22`
   */
  implicit def Tuple22Covariant[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ]: Covariant[
    ({
      type lambda[+x] =
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, x)
    })#lambda
  ] =
    new Covariant[
      ({
        type lambda[+x] =
          (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, x)
      })#lambda
    ] {
      override def map[A, B](
        f: A => B
      ): (
        (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A)
      ) => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, B) = {
        tuple =>
          (
            tuple._1,
            tuple._2,
            tuple._3,
            tuple._4,
            tuple._5,
            tuple._6,
            tuple._7,
            tuple._8,
            tuple._9,
            tuple._10,
            tuple._11,
            tuple._12,
            tuple._13,
            tuple._14,
            tuple._15,
            tuple._16,
            tuple._17,
            tuple._18,
            tuple._19,
            tuple._20,
            tuple._21,
            f(tuple._22)
          )
      }
    }

  /**
   * The `Covariant` instance for `ZIO`
   */
  implicit def ZIOCovariant[R, E]: Covariant[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def map[A, B](f: A => B): ZIO[R, E, A] => ZIO[R, E, B] = { zio =>
        zio.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZIO`
   */
  implicit def ZIOFailureCovariant[R, A]: Covariant[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[ZIO[R, E, A]] => Failure[ZIO[R, E1, A]] = { zio =>
        Failure.wrap(Failure.unwrap(zio).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `ZManaged`
   */
  implicit def ZManagedCovariant[R, E]: Covariant[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def map[A, B](f: A => B): ZManaged[R, E, A] => ZManaged[R, E, B] = { zmanaged =>
        zmanaged.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZManaged`
   */
  implicit def ZManagedFailureCovariant[R, A]: Covariant[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[ZManaged[R, E, A]] => Failure[ZManaged[R, E1, A]] = { zmanaged =>
        Failure.wrap(Failure.unwrap(zmanaged).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `ZStream`
   */
  implicit def ZStreamCovariant[R, E]: Covariant[({ type lambda[+o] = ZStream[R, E, o] })#lambda] =
    new Covariant[({ type lambda[+o] = ZStream[R, E, o] })#lambda] {
      def map[A, B](f: A => B): ZStream[R, E, A] => ZStream[R, E, B] = { ztream =>
        ztream.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZStream`
   */
  implicit def ZStreamFailureCovariant[R, O]: Covariant[({ type lambda[+e] = Failure[ZStream[R, e, O]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[ZStream[R, e, O]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[ZStream[R, E, O]] => Failure[ZStream[R, E1, O]] = { ztream =>
        Failure.wrap(Failure.unwrap(ztream).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `Schedule`
   */
  implicit def ScheduleCovariant[R, A]: Covariant[({ type lambda[+b] = Schedule[R, A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = Schedule[R, A, b] })#lambda] {
      def map[B, B1](f: B => B1): Schedule[R, A, B] => Schedule[R, A, B1] = { schedule =>
        schedule.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Fiber`
   */
  implicit def FiberCovariant[E]: Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def map[A, B](f: A => B): Fiber[E, A] => Fiber[E, B] = { fiber =>
        fiber.map(f)
      }
    }

  /**
   * The `Covariant` instance for `ZLayer`
   */
  implicit def ZLayerCovariant[R, E]: Covariant[({ type lambda[+rout] = ZLayer[R, E, rout] })#lambda] =
    new Covariant[({ type lambda[+rout] = ZLayer[R, E, rout] })#lambda] {
      def map[A, B](f: A => B): ZLayer[R, E, A] => ZLayer[R, E, B] = { zlayer =>
        zlayer.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZLayer`
   */
  implicit def ZLayerFailureCovariant[R, Out]: Covariant[({ type lambda[+e] = Failure[ZLayer[R, e, Out]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[ZLayer[R, e, Out]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[ZLayer[R, E, Out]] => Failure[ZLayer[R, E1, Out]] = { zlayer =>
        Failure.wrap(Failure.unwrap(zlayer).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `ZQueue`
   */
  implicit def ZQueueCovariant[RA, RB, EA, EB, A]
    : Covariant[({ type lambda[+b] = ZQueue[RA, RB, EA, EB, A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZQueue[RA, RB, EA, EB, A, b] })#lambda] {
      override def map[B, B1](f: B => B1): ZQueue[RA, RB, EA, EB, A, B] => ZQueue[RA, RB, EA, EB, A, B1] = { zqueue =>
        zqueue.map(f)
      }
    }

  /**
   * The `Covariant` instance for `Cause`
   */
  implicit def CauseCovariant: Covariant[Cause] = new Covariant[Cause] {
    override def map[A, B](f: A => B): Cause[A] => Cause[B] = { cause =>
      cause.map(f)
    }
  }

  /**
   * The `Covariant` instance for `Exit`
   */
  implicit def ExitCovariant[E]: Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new Covariant[({ type lambda[+a] = Exit[E, a] })#lambda] {
      override def map[A, B](f: A => B): Exit[E, A] => Exit[E, B] = { exit =>
        exit.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `Exit`
   */
  implicit def ExitFailureCovariant[A]: Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] =
    new Covariant[({ type lambda[+e] = Failure[Exit[e, A]] })#lambda] {
      override def map[E, E1](f: E => E1): Failure[Exit[E, A]] => Failure[Exit[E1, A]] = { exit =>
        Failure.wrap(Failure.unwrap(exit).mapError(f))
      }
    }

  /**
   * The `Covariant` instance for `ZRef`
   */
  implicit def ZRefCovariant[EA, EB, A]: Covariant[({ type lambda[+b] = ZRef[EA, EB, A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZRef[EA, EB, A, b] })#lambda] {
      override def map[B, C](f: B => C): ZRef[EA, EB, A, B] => ZRef[EA, EB, A, C] = { zref =>
        zref.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZRef` on its input
   */
  implicit def ZRefFailureInCovariant[EB, A, B]
    : Covariant[({ type lambda[+ea] = FailureIn[ZRef[ea, EB, A, B]] })#lambda] =
    new Covariant[({ type lambda[+ea] = FailureIn[ZRef[ea, EB, A, B]] })#lambda] {
      override def map[E, E1](f: E => E1): FailureIn[ZRef[E, EB, A, B]] => FailureIn[ZRef[E1, EB, A, B]] = { zref =>
        FailureIn.wrap(FailureIn.unwrap(zref).dimapError(f, identity))
      }
    }

  /**
   * The `Covariant` instance for a failed `ZRef` on its output
   */
  implicit def ZRefFailureOutCovariant[EA, A, B]
    : Covariant[({ type lambda[+eb] = FailureOut[ZRef[EA, eb, A, B]] })#lambda] =
    new Covariant[({ type lambda[+eb] = FailureOut[ZRef[EA, eb, A, B]] })#lambda] {
      override def map[E, E1](f: E => E1): FailureOut[ZRef[EA, E, A, B]] => FailureOut[ZRef[EA, E1, A, B]] = { zref =>
        FailureOut.wrap(FailureOut.unwrap(zref).dimapError(identity, f))
      }
    }

  /**
   * The `Covariant` instance for `ZRefM`
   */
  implicit def ZRefMCovariant[RA, RB, EA, EB, A]
    : Covariant[({ type lambda[+b] = ZRefM[RA, RB, EA, EB, A, b] })#lambda] =
    new Covariant[({ type lambda[+b] = ZRefM[RA, RB, EA, EB, A, b] })#lambda] {
      override def map[B, C](f: B => C): ZRefM[RA, RB, EA, EB, A, B] => ZRefM[RA, RB, EA, EB, A, C] = { zref =>
        zref.map(f)
      }
    }

  /**
   * The `Covariant` instance for a failed `ZRefM` on its input
   */
  implicit def ZRefMFailureInACovariant[RA, RB, EB, A, B]
    : Covariant[({ type lambda[+ea] = FailureIn[ZRefM[RA, RB, ea, EB, A, B]] })#lambda] =
    new Covariant[({ type lambda[+ea] = FailureIn[ZRefM[RA, RB, ea, EB, A, B]] })#lambda] {
      override def map[E, E1](
        f: E => E1
      ): FailureIn[ZRefM[RA, RB, E, EB, A, B]] => FailureIn[ZRefM[RA, RB, E1, EB, A, B]] = { zref =>
        FailureIn.wrap(FailureIn.unwrap(zref).dimapError(f, identity))
      }
    }

  /**
   * The `Covariant` instance for a failed `ZRefM` on its output
   */
  implicit def ZRefMFailureOutCovariant[RA, RB, EA, A, B]
    : Covariant[({ type lambda[+eb] = FailureOut[ZRefM[RA, RB, EA, eb, A, B]] })#lambda] =
    new Covariant[({ type lambda[+eb] = FailureOut[ZRefM[RA, RB, EA, eb, A, B]] })#lambda] {
      override def map[E, E1](
        f: E => E1
      ): FailureOut[ZRefM[RA, RB, EA, E, A, B]] => FailureOut[ZRefM[RA, RB, EA, E1, A, B]] = { zref =>
        FailureOut.wrap(FailureOut.unwrap(zref).dimapError(identity, f))
      }
    }
}

trait CovariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class CovariantOps[F[+_], A](private val self: F[A]) {
    def as[B](b: => B)(implicit F: Covariant[F]): F[B] = map(_ => b)

    def map[B](f: A => B)(implicit F: Covariant[F]): F[B] =
      F.map(f)(self)

    def unit(implicit F: Covariant[F]): F[Unit] = as(())
  }
}
