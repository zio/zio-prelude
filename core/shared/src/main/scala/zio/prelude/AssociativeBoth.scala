package zio.prelude

import zio._
import zio.prelude.coherent.AssociativeBothDeriveEqualInvariant
import zio.prelude.newtypes.{ AndF, BothF, Failure, NestedF, OrF }
import zio.stm.ZSTM
import zio.stream.{ ZSink, ZStream }
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.{ Success, Try }

/**
 * An associative binary operator that combines two values of types `F[A]`
 * and `F[B]` to produce an `F[(A, B)]`.
 */
@implicitNotFound("No implicit AssociativeBoth defined for ${F}.")
trait AssociativeBoth[F[_]] { self =>

  /**
   * Combines two values of types `F[A]` and `F[B]` to produce an `F[(A, B)]`.
   */
  def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]

  /** Combine with another `AssociativeBoth` to produce an `AssociativeBoth[(F[A], G[A])]`. */
  final def bothF[G[_]](g: AssociativeBoth[G]): AssociativeBoth[({ type lambda[A] = (F[A], G[A]) })#lambda] =
    new AssociativeBoth[({ type lambda[A] = (F[A], G[A]) })#lambda] {
      def both[A, B](faga: => (F[A], G[A]), fbgb: => (F[B], G[B])): (F[(A, B)], G[(A, B)]) =
        (self.both(faga._1, fbgb._1), g.both(faga._2, fbgb._2))
    }
}

object AssociativeBoth extends LawfulF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] {

  /**
   * Summons an implicit `AssociativeBoth[F]`.
   */
  def apply[F[_]](implicit associativeBoth: AssociativeBoth[F]): AssociativeBoth[F] =
    associativeBoth

  /**
   * For all `fa`, `fb`, and `fc`, `both(fa, both(fb, fc))` is equivalent
   * to `both(both(fa, fb), fc)`.
   */
  val associativityLaw: LawsF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] =
    new LawsF.Invariant.Law3[AssociativeBothDeriveEqualInvariant, Equal]("associativityLaw") {
      def apply[F[_]: AssociativeBothDeriveEqualInvariant, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        fb: F[B],
        fc: F[C]
      ): TestResult = {
        val left  = fa.zip(fb.zip(fc))
        val right = (fa.zip(fb)).zip(fc)
        val left2 = Invariant[F].invmap(Equivalence.tuple[A, B, C]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `AssociativeBoth` must satisfy.
   */
  val laws: LawsF.Invariant[AssociativeBothDeriveEqualInvariant, Equal] =
    associativityLaw

  def fromCovariantAssociativeFlatten[F[+_]](implicit
    covariant: Covariant[F],
    identityFlatten: AssociativeFlatten[F]
  ): AssociativeBoth[F] =
    new AssociativeBoth[F] {
      override def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = fa.map(a => fb.map(b => (a, b))).flatten
    }

  /**
   * Combines 2 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, B](
    a0: F[A0],
    a1: F[A1]
  )(
    f: (A0, A1) => B
  ): F[B] =
    (a0 <*> a1).map(f.tupled)

  /**
   * Combines 3 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2]
  )(
    f: (A0, A1, A2) => B
  ): F[B] =
    (a0 <*> a1 <*> a2).map { case ((a0, a1), a2) =>
      f(a0, a1, a2)
    }

  /**
   * Combines 4 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3]
  )(
    f: (A0, A1, A2, A3) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3).map { case (((a0, a1), a2), a3) =>
      f(a0, a1, a2, a3)
    }

  /**
   * Combines 5 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4]
  )(
    f: (A0, A1, A2, A3, A4) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4).map { case ((((a0, a1), a2), a3), a4) =>
      f(a0, a1, a2, a3, a4)
    }

  /**
   * Combines 6 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5]
  )(
    f: (A0, A1, A2, A3, A4, A5) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5).map { case (((((a0, a1), a2), a3), a4), a5) =>
      f(a0, a1, a2, a3, a4, a5)
    }

  /**
   * Combines 7 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6).map { case ((((((a0, a1), a2), a3), a4), a5), a6) =>
      f(a0, a1, a2, a3, a4, a5, a6)
    }

  /**
   * Combines 8 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7).map { case (((((((a0, a1), a2), a3), a4), a5), a6), a7) =>
      f(a0, a1, a2, a3, a4, a5, a6, a7)
    }

  /**
   * Combines 9 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8).map {
      case ((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }

  /**
   * Combines 10 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9).map {
      case (((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9) => f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }

  /**
   * Combines 11 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10).map {
      case ((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }

  /**
   * Combines 12 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11).map {
      case (((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    }

  /**
   * Combines 13 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12).map {
      case ((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }

  /**
   * Combines 14 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13).map {
      case (((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    }

  /**
   * Combines 15 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14).map {
      case ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    }

  /**
   * Combines 16 `F` values using the provided function `f`.
   */
  def mapN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15).map {
      case (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    }

  /**
   * Combines 17 `F` values using the provided function `f`.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16).map {
      case ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    }

  /**
   * Combines 18 `F` values into a tuple in maps the result with the provided function.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16 <*> a17).map {
      case (
            ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
            a17
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    }

  /**
   * Combines 19 `F` values using the provided function `f`.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16 <*> a17 <*> a18).map {
      case (
            (
              ((((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
              a17
            ),
            a18
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    }

  /**
   * Combines 20 `F` values using the provided function `f`.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16 <*> a17 <*> a18 <*> a19).map {
      case (
            (
              (
                (
                  (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                  a16
                ),
                a17
              ),
              a18
            ),
            a19
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    }

  /**
   * Combines 21 `F` values using the provided function `f`.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16 <*> a17 <*> a18 <*> a19 <*> a20).map {
      case (
            (
              (
                (
                  (
                    (((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                    a16
                  ),
                  a17
                ),
                a18
              ),
              a19
            ),
            a20
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    }

  /**
   * Combines 22 `F` values using the provided function `f`.
   */
  def mapN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20],
    a21: F[A21]
  )(
    f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B
  ): F[B] =
    (a0 <*> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8 <*> a9 <*> a10 <*> a11 <*> a12 <*> a13 <*> a14 <*> a15 <*> a16 <*> a17 <*> a18 <*> a19 <*> a20 <*> a21).map {
      case (
            (
              (
                (
                  (
                    (
                      (
                        ((((((((((((((a0, a1), a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14),
                        a15
                      ),
                      a16
                    ),
                    a17
                  ),
                  a18
                ),
                a19
              ),
              a20
            ),
            a21
          ) =>
        f(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
    }

  /**
   * Combines 2 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1](
    a0: F[A0],
    a1: F[A1]
  ): F[(A0, A1)] =
    mapN(a0, a1)((_, _))

  /**
   * Combines 3 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2]
  ): F[(A0, A1, A2)] =
    mapN(a0, a1, a2)((_, _, _))

  /**
   * Combines 4 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3]
  ): F[(A0, A1, A2, A3)] =
    mapN(a0, a1, a2, a3)((_, _, _, _))

  /**
   * Combines 5 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4]
  ): F[(A0, A1, A2, A3, A4)] =
    mapN(a0, a1, a2, a3, a4)((_, _, _, _, _))

  /**
   * Combines 6 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5]
  ): F[(A0, A1, A2, A3, A4, A5)] =
    mapN(a0, a1, a2, a3, a4, a5)((_, _, _, _, _, _))

  /**
   * Combines 7 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6]
  ): F[(A0, A1, A2, A3, A4, A5, A6)] =
    mapN(a0, a1, a2, a3, a4, a5, a6)((_, _, _, _, _, _, _))

  /**
   * Combines 8 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7)((_, _, _, _, _, _, _, _))

  /**
   * Combines 9 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8)((_, _, _, _, _, _, _, _, _))

  /**
   * Combines 10 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)((_, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 11 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)((_, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 12 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)((_, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 13 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)((_, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 14 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 15 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  /**
   * Combines 16 `F` values into a tuple.
   */
  def tupleN[F[+_]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 17 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 18 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 19 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 20 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 21 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * Combines 22 `F` values into a tuple.
   */
  def tupleN[F[
    +_
  ]: AssociativeBoth: Covariant, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4],
    a5: F[A5],
    a6: F[A6],
    a7: F[A7],
    a8: F[A8],
    a9: F[A9],
    a10: F[A10],
    a11: F[A11],
    a12: F[A12],
    a13: F[A13],
    a14: F[A14],
    a15: F[A15],
    a16: F[A16],
    a17: F[A17],
    a18: F[A18],
    a19: F[A19],
    a20: F[A20],
    a21: F[A21]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    mapN(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(
      (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    )

  /**
   * The `AssociativeBoth` instance for `Chunk`.
   */
  implicit def ChunkAssociativeBoth: AssociativeBoth[Chunk] =
    new AssociativeBoth[Chunk] {
      def both[A, B](fa: => Chunk[A], fb: => Chunk[B]): Chunk[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for `Either`.
   */
  implicit def EitherIdentityBoth[L]: IdentityBoth[({ type lambda[+r] = Either[L, r] })#lambda] =
    new IdentityBoth[({ type lambda[+r] = Either[L, r] })#lambda] {
      val any: Either[L, Any] = Right(())

      def both[A, B](fa: => Either[L, A], fb: => Either[L, B]): Either[L, (A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for a failed `Either`
   */
  implicit def EitherFailedIdentityBoth[R]: IdentityBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] =
    new IdentityBoth[({ type lambda[+l] = Failure[Either[l, R]] })#lambda] {
      val any: Failure[Either[Any, R]] = Failure.wrap(Left(()))

      def both[A, B](fa: => Failure[Either[A, R]], fb: => Failure[Either[B, R]]): Failure[Either[(A, B), R]] =
        Failure.wrap {
          Failure
            .unwrap(fa)
            .left
            .flatMap(a => Failure.unwrap(fb).left.map(b => (a, b)))
        }
    }

  /**
   * The `AssociativeBoth` instance for `Exit`.
   */
  implicit def ExitAssociativeBoth[E]: AssociativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = Exit[E, a] })#lambda] {
      def both[A, B](fa: => Exit[E, A], fb: => Exit[E, B]): Exit[E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `Fiber`.
   */
  implicit def FiberAssociativeBoth[E]: AssociativeBoth[({ type lambda[+a] = Fiber[E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = Fiber[E, a] })#lambda] {
      def both[A, B](fa: => Fiber[E, A], fb: => Fiber[E, B]): Fiber[E, (A, B)] = fa zip fb
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for `Future`.
   */
  implicit val FutureIdentityBoth: IdentityBoth[Future] =
    new IdentityBoth[Future] {
      val any: Future[Any] = Future.successful(())

      def both[A, B](fa: => Future[A], fb: => Future[B]): Future[(A, B)] = fa zip fb
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for `Id`.
   */
  implicit val IdIdentityBoth: IdentityBoth[Id] =
    new IdentityBoth[Id] {
      val any: Id[Any] = Id(())

      def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] =
        Id(Id.unwrap(fa) -> Id.unwrap(fb))
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for `List`.
   */
  implicit val ListIdentityBoth: IdentityBoth[List] =
    new IdentityBoth[List] {
      val any: List[Any] =
        List(())

      def both[A, B](fa: => List[A], fb: => List[B]): List[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  final def composeF[F[+_]: IdentityBoth: Covariant, G[+_]](implicit
    G: IdentityBoth[G]
  ): IdentityBoth[({ type lambda[+A] = F[G[A]] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = F[G[A]] })#lambda] {
      def any: F[G[Any]] =
        G.any.succeed[F]

      def both[A, B](fa: => F[G[A]], fb: => F[G[B]]): F[G[(A, B)]] =
        fa.zipWith(fb)(_ zip _)
    }

  final def bothF[F[+_], G[+_]](implicit
    F: IdentityBoth[F],
    G: IdentityBoth[G]
  ): IdentityBoth[({ type lambda[+A] = (F[A], G[A]) })#lambda] =
    new IdentityBoth[({ type lambda[+A] = (F[A], G[A]) })#lambda] {
      private val bothFG = F.bothF[G](G)

      def any: (F[Any], G[Any]) =
        (F.any, G.any)

      def both[A, B](fa: => (F[A], G[A]), fb: => (F[B], G[B])): (F[(A, B)], G[(A, B)]) =
        bothFG.both(fa, fb)
    }

  implicit def NestedFIdentityBoth[F[+_]: IdentityBoth: Covariant, G[+_]: IdentityBoth]
    : IdentityBoth[({ type lambda[+A] = NestedF[F, G, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = NestedF[F, G, A] })#lambda] {
      private val FG = composeF[F, G]

      def any: NestedF[F, G, Any] =
        NestedF(FG.any)

      def both[A, B](fa: => NestedF[F, G, A], fb: => NestedF[F, G, B]): NestedF[F, G, (A, B)] =
        NestedF(FG.both(NestedF.unwrap[F[G[A]]](fa), NestedF.unwrap[F[G[B]]](fb)))
    }

  implicit def BothFIdentityBoth[F[+_], G[+_]](implicit
    F: IdentityBoth[F],
    G: IdentityBoth[G]
  ): IdentityBoth[({ type lambda[+A] = BothF[F, G, A] })#lambda] =
    new IdentityBoth[({ type lambda[+A] = BothF[F, G, A] })#lambda] {
      private val FG = AssociativeBoth.bothF[F, G]

      def any: BothF[F, G, Any] =
        BothF(FG.any)

      def both[A, B](fga: => BothF[F, G, A], fgb: => BothF[F, G, B]): BothF[F, G, (A, B)] =
        BothF(FG.both(BothF.unwrap(fga), BothF.unwrap(fgb)))
    }

  /**
   * The `AssociativeBoth` instance for `NonEmptyChunk`.
   */
  implicit def NonEmptyChunkAssociativeBoth: AssociativeBoth[NonEmptyChunk] =
    new AssociativeBoth[NonEmptyChunk] {
      def both[A, B](fa: => NonEmptyChunk[A], fb: => NonEmptyChunk[B]): NonEmptyChunk[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `IdentityBoth` (with `AssociativeBoth`) instance for `Option`.
   */
  implicit val OptionIdentityBoth: IdentityBoth[Option] =
    new IdentityBoth[Option] {
      val any: Option[Any] =
        Some(())

      def both[A, B](fa: => Option[A], fb: => Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }

  /**
   * The `AssociativeBoth` instance for And `Schedule`.
   */
  implicit def ScheduleAndAssociativeBoth[R, E]
    : AssociativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = AndF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => AndF[Schedule[R, E, A]], fb: => AndF[Schedule[R, E, B]]): AndF[Schedule[R, E, (A, B)]] =
        AndF.wrap {
          AndF.unwrap(fa) && AndF.unwrap(fb)
        }
    }

  /**
   * The `AssociativeBoth` instance for Or `Schedule`.
   */
  implicit def ScheduleOrAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = OrF[Schedule[R, E, a]] })#lambda] {
      def both[A, B](fa: => OrF[Schedule[R, E, A]], fb: => OrF[Schedule[R, E, B]]): OrF[Schedule[R, E, (A, B)]] =
        OrF.wrap {
          OrF.unwrap(fa) || OrF.unwrap(fb)
        }
    }

  /**
   * The `IdentityBoth` (and `AssociativeBoth`) instance for `Try`.
   */
  implicit val TryIdentityBoth: IdentityBoth[Try] =
    new IdentityBoth[Try] {
      val any: Try[Any] = Success(())

      def both[A, B](fa: => Try[A], fb: => Try[B]): Try[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `AssociativeBoth` instance for `Vector`.
   */
  implicit def VectorAssociativeBoth: AssociativeBoth[Vector] =
    new AssociativeBoth[Vector] {
      def both[A, B](fa: => Vector[A], fb: => Vector[B]): Vector[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
    }

  /**
   * The `AssociativeBoth` instance for `ZIO`.
   */
  implicit def ZIOAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZIO[R, E, a] })#lambda] {
      def both[A, B](fa: => ZIO[R, E, A], fb: => ZIO[R, E, B]): ZIO[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for failed `ZIO`.
   */
  implicit def ZIOFailureAssociativeBoth[R, A]: AssociativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] =
    new AssociativeBoth[({ type lambda[+e] = Failure[ZIO[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZIO[R, EA, A]],
        fb: => Failure[ZIO[R, EB, A]]
      ): Failure[ZIO[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zip Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `AssociativeBoth` instance for `ZLayer`.
   */
  implicit def ZLayerAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZLayer[R, E, a] })#lambda] {
      def both[A, B](fa: => ZLayer[R, E, A], fb: => ZLayer[R, E, B]): ZLayer[R, E, (A, B)] = fa zipPar fb
    }

  /**
   * The `AssociativeBoth` instance for `ZManaged`.
   */
  implicit def ZManagedAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZManaged[R, E, a] })#lambda] {
      def both[A, B](fa: => ZManaged[R, E, A], fb: => ZManaged[R, E, B]): ZManaged[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for failed `ZManaged`.
   */
  implicit def ZManagedFailureAssociativeBoth[R, A]
    : AssociativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] =
    new AssociativeBoth[({ type lambda[+e] = Failure[ZManaged[R, e, A]] })#lambda] {
      def both[EA, EB](
        fa: => Failure[ZManaged[R, EA, A]],
        fb: => Failure[ZManaged[R, EB, A]]
      ): Failure[ZManaged[R, (EA, EB), A]] =
        Failure.wrap {
          (Failure.unwrap(fa).flip zip Failure.unwrap(fb).flip).flip
        }
    }

  /**
   * The `AssociativeBoth` instance for `ZSink`.
   */
  implicit def ZSinkAssociativeBoth[R, E, I]: AssociativeBoth[({ type lambda[+a] = ZSink[R, E, I, I, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZSink[R, E, I, I, a] })#lambda] {
      def both[A, B](fa: => ZSink[R, E, I, I, A], fb: => ZSink[R, E, I, I, B]): ZSink[R, E, I, I, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `ZSTM`.
   */
  implicit def ZSTMAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZSTM[R, E, a] })#lambda] {
      def both[A, B](fa: => ZSTM[R, E, A], fb: => ZSTM[R, E, B]): ZSTM[R, E, (A, B)] = fa zip fb
    }

  /**
   * The `AssociativeBoth` instance for `ZStream`.
   */
  implicit def ZStreamAssociativeBoth[R, E]: AssociativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] =
    new AssociativeBoth[({ type lambda[+a] = ZStream[R, E, a] })#lambda] {
      def both[A, B](fa: => ZStream[R, E, A], fb: => ZStream[R, E, B]): ZStream[R, E, (A, B)] = fa cross fb
    }
}

trait AssociativeBothSyntax {

  /**
   * Provides infix syntax for associative operations for invariant types.
   */
  implicit class AssociativeBothOps[F[_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zip`.
     */
    def <*>[B](fb: => F[B])(implicit both: AssociativeBoth[F]): F[(A, B)] =
      zip(fb)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`.
     */
    def zip[B](fb: => F[B])(implicit both: AssociativeBoth[F]): F[(A, B)] =
      both.both(fa, fb)
  }

  /**
   * Provides infix syntax for associative operations for covariant types.
   */
  implicit class AssociativeBothCovariantOps[F[+_], A](fa: => F[A]) {

    /**
     * A symbolic alias for `zipLeft`.
     */
    def <*[B](fb: => F[B])(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[A] =
      zipLeft(fb)

    /**
     * A symbolic alias for `zipRight`.
     */
    def *>[B](fb: => F[B])(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[B] =
      zipRight(fb)

    /**
     * Combines an `F[A]` value with itself using `zipRight` forever.
     */
    def forever(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[A] =
      fa *> forever

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`, keeping only the left value.
     */
    def zipLeft[B](fb: => F[B])(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[A] =
      zipWith(fb)((a, _) => a)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]`, keeping only the right value.
     */
    def zipRight[B](fb: => F[B])(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[B] =
      zipWith(fb)((_, b) => b)

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then maps the result with the specified function.
     */
    def zipWith[B, C](fb: => F[B])(f: (A, B) => C)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[C] =
      both.both(fa, fb).map(f.tupled)
  }

  /**
   * Provides infix syntax for associative operations for contravariant types.
   */
  implicit class AssociativeBothContravariantOps[F[-_], A](fa: => F[A]) {

    /**
     * Combines two values of types `F[A]` and `F[B]` to produce an
     * `F[(A, B)]` and then contramaps the result with the specified function.
     */
    def bothWith[B, C](
      fb: => F[B]
    )(f: C => (A, B))(implicit both: AssociativeBoth[F], contravariant: Contravariant[F]): F[C] =
      both.both(fa, fb).contramap(f)
  }

  implicit class AssociativeBothTuple2Ops[F[+_], T1, T2](tf: => (F[T1], F[T2])) {
    def mapN[R](f: (T1, T2) => R)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2])).tupled(tf)
  }

  implicit class AssociativeBothTuple3Ops[F[+_], T1, T2, T3](tf: => (F[T1], F[T2], F[T3])) {
    def mapN[R](f: (T1, T2, T3) => R)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3])).tupled(tf)
  }

  implicit class AssociativeBothTuple4Ops[F[+_], T1, T2, T3, T4](tf: => (F[T1], F[T2], F[T3], F[T4])) {
    def mapN[R](f: (T1, T2, T3, T4) => R)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4])).tupled(tf)
  }

  implicit class AssociativeBothTuple5Ops[F[+_], T1, T2, T3, T4, T5](tf: => (F[T1], F[T2], F[T3], F[T4], F[T5])) {
    def mapN[R](f: (T1, T2, T3, T4, T5) => R)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5])).tupled(tf)
  }

  implicit class AssociativeBothTuple6Ops[F[+_], T1, T2, T3, T4, T5, T6](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6])
  ) {
    def mapN[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6])).tupled(tf)
  }

  implicit class AssociativeBothTuple7Ops[F[+_], T1, T2, T3, T4, T5, T6, T7](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth.mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7])(f)).tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7)] =
      (AssociativeBoth.tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7])).tupled(tf)
  }

  implicit class AssociativeBothTuple8Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8])(f))
        .tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      (AssociativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8]))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple9Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9])(f))
        .tupled(tf)

    def tupleN(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      (AssociativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9]))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple10Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9], _: F[T10])(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      (AssociativeBoth
        .tupleN(_: F[T1], _: F[T2], _: F[T3], _: F[T4], _: F[T5], _: F[T6], _: F[T7], _: F[T8], _: F[T9], _: F[T10]))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple11Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      (AssociativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11]
        ))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple12Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      (AssociativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12]
        ))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple13Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      (AssociativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13]
        ))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple14Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    tf: => (F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14])
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      (AssociativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14]
        ))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple15Ops[F[+_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      (AssociativeBoth
        .tupleN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15]
        ))
        .tupled(tf)
  }

  implicit class AssociativeBothTuple16Ops[
    F[+_],
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
    T16
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple17Ops[
    F[+_],
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
    T17
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple18Ops[
    F[+_],
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
    T18
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple19Ops[
    F[+_],
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
    T19
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple20Ops[
    F[+_],
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
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple21Ops[
    F[+_],
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
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20],
      F[T21]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20],
          _: F[T21]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20],
            _: F[T21]
          )
        )
        .tupled(tf)
  }

  implicit class AssociativeBothTuple22Ops[
    F[+_],
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
  ](
    tf: => (
      F[T1],
      F[T2],
      F[T3],
      F[T4],
      F[T5],
      F[T6],
      F[T7],
      F[T8],
      F[T9],
      F[T10],
      F[T11],
      F[T12],
      F[T13],
      F[T14],
      F[T15],
      F[T16],
      F[T17],
      F[T18],
      F[T19],
      F[T20],
      F[T21],
      F[T22]
    )
  ) {
    def mapN[R](
      f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R
    )(implicit both: AssociativeBoth[F], covariant: Covariant[F]): F[R] =
      (AssociativeBoth
        .mapN(
          _: F[T1],
          _: F[T2],
          _: F[T3],
          _: F[T4],
          _: F[T5],
          _: F[T6],
          _: F[T7],
          _: F[T8],
          _: F[T9],
          _: F[T10],
          _: F[T11],
          _: F[T12],
          _: F[T13],
          _: F[T14],
          _: F[T15],
          _: F[T16],
          _: F[T17],
          _: F[T18],
          _: F[T19],
          _: F[T20],
          _: F[T21],
          _: F[T22]
        )(f))
        .tupled(tf)

    def tupleN(implicit
      both: AssociativeBoth[F],
      covariant: Covariant[F]
    ): F[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
      (
        AssociativeBoth
          .tupleN(
            _: F[T1],
            _: F[T2],
            _: F[T3],
            _: F[T4],
            _: F[T5],
            _: F[T6],
            _: F[T7],
            _: F[T8],
            _: F[T9],
            _: F[T10],
            _: F[T11],
            _: F[T12],
            _: F[T13],
            _: F[T14],
            _: F[T15],
            _: F[T16],
            _: F[T17],
            _: F[T18],
            _: F[T19],
            _: F[T20],
            _: F[T21],
            _: F[T22]
          )
        )
        .tupled(tf)
  }
}
