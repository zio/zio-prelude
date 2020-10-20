package zio.prelude

import zio.prelude.coherent.{ AnnihilatingEqual, DistributiveEqual }
import zio.prelude.newtypes.{ Prod, Sum }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Ringoid[A, +AdditionClosure[x] <: Associative[x], +MultiplicationClosure[x] <: Associative[x]] {

  def add(l: => A, r: => A): A =
    Sum.unwrap(ClosureAddition.combine(Sum(l), Sum(r)))

  def multiply(l: => A, r: => A): A =
    Prod.unwrap(ClosureMultiplication.combine(Prod(l), Prod(r)))

  def ClosureAddition: AdditionClosure[Sum[A]]

  def ClosureMultiplication: MultiplicationClosure[Prod[A]]
}

object Ringoid {

  type IntAddition[x]       = Commutative[x] with Inverse[x]
  type IntMultiplication[x] = Commutative[x] with Identity[x]

  implicit val IntDistributive
    : Annihilating[Int, IntAddition, IntMultiplication] with Distributive[Int, IntAddition, IntMultiplication] =
    new Annihilating[Int, IntAddition, IntMultiplication] with Distributive[Int, IntAddition, IntMultiplication] {

      override def ClosureAddition: Commutative[Sum[Int]] with Inverse[Sum[Int]] =
        Associative.IntSumCommutativeInverse

      override def ClosureMultiplication: Commutative[Prod[Int]] with Identity[Prod[Int]] =
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

trait Annihilating[A, +AdditionClosure[x] <: Identity[x], +MultiplicationClosure[x] <: Associative[x]]
    extends Ringoid[A, AdditionClosure, MultiplicationClosure]

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
        (Sum.unwrap(A.ClosureAddition.identity) *** a) <-> Sum.unwrap(A.ClosureAddition.identity)
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
        (a *** Sum.unwrap(A.ClosureAddition.identity)) <-> Sum.unwrap(A.ClosureAddition.identity)
    }

  /**
   * The set of all laws that instances of `Annihilating` must satisfy.
   */
  val laws: Laws[AnnihilatingEqual] =
    leftAnnihilationLaw + rightAnnihilationLaw

  /**
   * Summons an implicit `Annihilating[A]`.
   */
  def apply[A, Add[x] <: Identity[x], Mult[x] <: Associative[x]](implicit
    annihilating: Annihilating[A, Add, Mult]
  ): Annihilating[A, Add, Mult] =
    annihilating
}

trait Distributive[A, +AdditionClosure[x] <: Associative[x], +MultiplicationClosure[x] <: Associative[x]]
    extends Ringoid[A, AdditionClosure, MultiplicationClosure]

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
  def apply[A, Add[x] <: Associative[x], Mult[x] <: Associative[x]](implicit
    distributive: Distributive[A, Add, Mult]
  ): Distributive[A, Add, Mult] =
    distributive
}
