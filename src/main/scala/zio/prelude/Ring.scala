package zio.prelude

import zio.prelude.coherent.{ AnnihilatingEqual, DistributiveEqual }
import zio.prelude.newtypes.{ Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Ringoid[A, +Addition[x] <: Associative[x], +Multiplication[x] <: Associative[x]] {

  def add(l: => A, r: => A): A =
    Sum.unwrap(Addition.combine(Sum(l), Sum(r)))

  def multiply(l: => A, r: => A): A =
    Prod.unwrap(Multiplication.combine(Prod(l), Prod(r)))

  def Addition: Addition[Sum[A]]

  def Multiplication: Multiplication[Prod[A]]
}

object Ringoid {

  type IntAddition[x]       = Commutative[x] with Inverse[x]
  type IntMultiplication[x] = Commutative[x] with Identity[x]

  implicit val IntDistributive
    : Annihilating[Int, IntAddition, IntMultiplication] with Distributive[Int, IntAddition, IntMultiplication] =
    new Annihilating[Int, IntAddition, IntMultiplication] with Distributive[Int, IntAddition, IntMultiplication] {

      override def add(l: => Int, r: => Int): Int = l + r

      override def multiply(l: => Int, r: => Int): Int = l * r

      override def Addition: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
        Associative.IntSumCommutativeInverse

      override def Multiplication: Commutative[Prod[Int]] with Identity[Prod[Int]] =
        Associative.IntProdCommutativeIdentity
    }
}

trait RingoidSyntax {

  /**
   * Provides infix syntax for adding or multiplying two values.
   */
  implicit class RingoidOps[A](l: A) {

    /**
     * A symbolic alias for `add`.
     */
    def +++(r: => A)(implicit associative: Ringoid[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * Add two values.
     */
    def add(r: => A)(implicit associative: Ringoid[A, Associative, Associative]): A =
      associative.add(l, r)

    /**
     * A symbolic alias for `multiply`.
     */
    def ***(r: => A)(implicit associative: Ringoid[A, Associative, Associative]): A =
      associative.multiply(l, r)

    /**
     * Multiply two values.
     */
    def multiply(r: => A)(implicit associative: Ringoid[A, Associative, Associative]): A =
      associative.multiply(l, r)
  }

}

trait Annihilating[A, +Addition[x] <: Identity[x], +Multiplication[x] <: Associative[x]]
    extends Ringoid[A, Addition, Multiplication]

object Annihilating extends Lawful[AnnihilatingEqual] {

  /**
   * The left annihilation law states that for the multiplication operator `*`
   * and 0, the identity value for addition, for any value `a`, the following must hold:
   *
   * {{{
   * 0 * a === 0
   * }}}
   */
  val leftAnnihilationLaw: Laws[AnnihilatingEqual] =
    new Laws.Law1[AnnihilatingEqual]("leftAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilatingEqual[A]): TestResult =
        (Sum.unwrap(A.Addition.identity) *** a) <-> Sum.unwrap(A.Addition.identity)
    }

  /**
   * The right annihilation law states that for the multiplication operator `*`
   * and 0, the identity value for addition, for any value `a`, the following must hold:
   *
   * {{{
   * a * 0 === 0
   * }}}
   */
  val rightAnnihilationLaw: Laws[AnnihilatingEqual] =
    new Laws.Law1[AnnihilatingEqual]("rightAnnihilationLaw") {
      def apply[A](a: A)(implicit A: AnnihilatingEqual[A]): TestResult =
        (a *** Sum.unwrap(A.Addition.identity)) <-> Sum.unwrap(A.Addition.identity)
    }

  /**
   * The set of all laws that instances of `Annihilating` must satisfy.
   */
  val laws: Laws[AnnihilatingEqual] =
    leftAnnihilationLaw + rightAnnihilationLaw

  /**
   * Summons an implicit `Annihilating[A]`.
   */
  def apply[A, Addition[x] <: Identity[x], Multiplication[x] <: Associative[x]](implicit
    annihilating: Annihilating[A, Addition, Multiplication]
  ): Annihilating[A, Addition, Multiplication] =
    annihilating
}

trait Distributive[A, +Addition[x] <: Associative[x], +Multiplication[x] <: Associative[x]]
    extends Ringoid[A, Addition, Multiplication]

object Distributive extends Lawful[DistributiveEqual] {

  /**
   * The left distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * a1 * (a2 + a3) === (a1 * a2) + (a1 * a3)
   * }}}
   */
  val leftDistributivityLaw: Laws[DistributiveEqual] =
    new Laws.Law3[DistributiveEqual]("leftDistributivityLaw") {
      def apply[A: DistributiveEqual](a1: A, a2: A, a3: A): TestResult =
        (a1 *** (a2 +++ a3)) <-> ((a1 *** a2) +++ (a1 *** a3))
    }

  /**
   * The right distributivity law states that for operators `+` and `*`, for all
   * values `a1`, `a2`, and `a3`, the following must hold:
   *
   * {{{
   * (a1 + a2) * a3 === (a1 * a3) + (a2 * a3)
   * }}}
   */
  val rightDistributivityLaw: Laws[DistributiveEqual] =
    new Laws.Law3[DistributiveEqual]("rightDistributivityLaw") {
      def apply[A: DistributiveEqual](a1: A, a2: A, a3: A): TestResult =
        ((a1 +++ a2) *** a3) <-> ((a1 *** a3) +++ (a2 *** a3))
    }

  /**
   * The set of all laws that instances of `Distributive` must satisfy.
   */
  val laws: Laws[DistributiveEqual] =
    leftDistributivityLaw + rightDistributivityLaw

  /**
   * Summons an implicit `Distributive[A]`.
   */
  def apply[A, Addition[x] <: Associative[x], Multiplication[x] <: Associative[x]](implicit
    distributive: Distributive[A, Addition, Multiplication]
  ): Distributive[A, Addition, Multiplication] =
    distributive
}
