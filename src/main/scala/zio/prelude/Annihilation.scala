package zio.prelude

import zio.prelude.coherent.AnnihilationEqual
import zio.prelude.newtypes.{ Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Annihilation[A, +Addition[x] <: Identity[x], +Multiplication[x] <: Associative[x]]
    extends AddMultiplyShape[A, Addition, Multiplication] {
  def annihilation: A = Sum.unwrap(Addition.identity)
}

object Annihilation extends Lawful[AnnihilationEqual] {

  /**
   * The left annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * 0 * a === 0
   * }}}
   */
  val leftAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("leftAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (A.annihilation *** a) <-> A.annihilation
    }

  /**
   * The right annihilation law states that for the multiplication operator `*`,
   * 0 (the identity value for addition) and for any value `a`, the following must hold:
   *
   * {{{
   * a * 0 === 0
   * }}}
   */
  val rightAnnihilationLaw: Laws[AnnihilationEqual] =
    new Laws.Law1[AnnihilationEqual]("rightAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilationEqual[A]): TestResult =
        (a *** A.annihilation) <-> A.annihilation
    }

  /**
   * The set of all laws that instances of `Annihilation` must satisfy.
   */
  val laws: Laws[AnnihilationEqual] =
    leftAnnihilationLaw + rightAnnihilationLaw

  /**
   * Summons an implicit `Annihilation[A]`.
   */
  def apply[A, Addition[x] <: Identity[x], Multiplication[x] <: Associative[x]](implicit
    annihilatingZero: Annihilation[A, Addition, Multiplication]
  ): Annihilation[A, Addition, Multiplication] =
    annihilatingZero

  def fromAdditiveInverse[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    ev: DistributiveMultiply[A, Addition, Multiplication]
  ): Annihilation[A, Addition, Multiplication]
    with DistributiveMultiply[A, Addition, Multiplication]
    with SubtractShape[A, Addition, Multiplication] = SubtractShape.fromAdditiveInverseAndDistributiveMultiply(ev)

  def fromSubtract[A, Addition[x] <: Inverse[x], Multiplication[x] <: Associative[x]](implicit
    distributive0: DistributiveMultiply[A, Addition, Multiplication],
    subtract0: SubtractShape[A, Addition, Multiplication]
  ): Annihilation[A, Addition, Multiplication]
    with DistributiveMultiply[A, Addition, Multiplication]
    with SubtractShape[A, Addition, Multiplication] =
    new Annihilation[A, Addition, Multiplication]
      with DistributiveMultiply[A, Addition, Multiplication]
      with SubtractShape[A, Addition, Multiplication] {

      override def add(l: => A, r: => A): A = distributive0.add(l, r)

      override def multiply(l: => A, r: => A): A = distributive0.multiply(l, r)

      override def subtract(l: => A, r: => A): A = subtract0.subtract(l, r)

      override def Addition: Addition[Sum[A]] = distributive0.Addition

      override def Multiplication: Multiplication[Prod[A]] = distributive0.Multiplication
    }
}
