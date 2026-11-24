package zio.prelude.experimental.coherent

import zio.prelude._
import zio.prelude.experimental._
import zio.prelude.newtypes.{AndF, OrF, Prod, Sum}

/**
 * An `Absorption` and an `Equal` for the same type in one value. The laws in
 * `AbsorptionLaws`, in zio-prelude-experimental-laws, are stated for this type.
 * It is experimental, like `Absorption`.
 */
trait AbsorptionEqual[A] extends Absorption[A] with Equal[A]

/**
 * Companion of [[AbsorptionEqual]], with its implicit derivation.
 */
object AbsorptionEqual {

  /**
   * Combines the implicit `Absorption[A]` and `Equal[A]` into an
   * `AbsorptionEqual[A]` that delegates to them.
   */
  implicit def derive[A](implicit absorption0: Absorption[A], equal0: Equal[A]): AbsorptionEqual[A] =
    new AbsorptionEqual[A] {

      override def or(l: => A, r: => A): A = absorption0.or(l, r)

      override def and(l: => A, r: => A): A = absorption0.and(l, r)

      override def Or: Associative[OrF[A]] = absorption0.Or

      override def And: Associative[AndF[A]] = absorption0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An `Annihilation` and an `Equal` for the same type in one value. The laws in
 * `AnnihilationLaws`, in zio-prelude-experimental-laws, are stated for this
 * type. It is experimental, like `Annihilation`.
 */
trait AnnihilationEqual[A] extends Annihilation[A] with DistributiveProdEqual[A]

/**
 * Companion of [[AnnihilationEqual]], with its implicit derivation.
 */
object AnnihilationEqual {

  /**
   * Combines the implicit `Annihilation[A]` and `Equal[A]` into an
   * `AnnihilationEqual[A]` that delegates `sum`, `prod`, `Sum`, `Prod` and
   * equality to them. It does not forward `annihilation`, so the result uses
   * the identity of `Sum` even when the given instance overrides
   * `annihilation`.
   */
  implicit def derive[A](implicit annihilation0: Annihilation[A], equal0: Equal[A]): AnnihilationEqual[A] =
    new AnnihilationEqual[A] {

      override def sum(l: => A, r: => A): A = annihilation0.sum(l, r)

      override def prod(l: => A, r: => A): A = annihilation0.prod(l, r)

      override def Sum: Identity[Sum[A]] = annihilation0.Sum

      override def Prod: Associative[Prod[A]] = annihilation0.Prod

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * A `DistributiveAbsorption` and an `Equal` for the same type in one value. The
 * laws in `DistributiveAbsorptionLaws`, in zio-prelude-experimental-laws, are
 * stated for this type. It is experimental, like `DistributiveAbsorption`.
 */
trait DistributiveAbsorptionEqual[A] extends AbsorptionEqual[A] with DistributiveAbsorption[A]

/**
 * Companion of [[DistributiveAbsorptionEqual]], with its implicit derivation.
 */
object DistributiveAbsorptionEqual {

  /**
   * Combines the implicit `DistributiveAbsorption[A]` and `Equal[A]` into a
   * `DistributiveAbsorptionEqual[A]` that delegates to them.
   */
  implicit def derive[A](implicit
    distributiveJoinMeet0: DistributiveAbsorption[A],
    equal0: Equal[A]
  ): DistributiveAbsorptionEqual[A] =
    new DistributiveAbsorptionEqual[A] {

      override def or(l: => A, r: => A): A = distributiveJoinMeet0.or(l, r)

      override def and(l: => A, r: => A): A = distributiveJoinMeet0.and(l, r)

      override def Or: Associative[OrF[A]] = distributiveJoinMeet0.Or

      override def And: Associative[AndF[A]] = distributiveJoinMeet0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An `ExcludedMiddle` and an `Equal` for the same type in one value. The laws
 * in `ExcludedMiddleLaws`, in zio-prelude-experimental-laws, are stated for
 * this type. It is experimental, like `ExcludedMiddle`.
 */
trait ExcludedMiddleEqual[A] extends AbsorptionEqual[A] with ExcludedMiddle[A]

/**
 * Companion of [[ExcludedMiddleEqual]], with its implicit derivation.
 */
object ExcludedMiddleEqual {

  /**
   * Combines the implicit `ExcludedMiddle[A]` and `Equal[A]` into an
   * `ExcludedMiddleEqual[A]` that delegates to them.
   */
  implicit def derive[A](implicit excludedMiddle0: ExcludedMiddle[A], equal0: Equal[A]): ExcludedMiddleEqual[A] =
    new ExcludedMiddleEqual[A] {

      override def top: A = excludedMiddle0.top

      override def complement(a: => A): A = excludedMiddle0.complement(a)

      override def or(l: => A, r: => A): A = excludedMiddle0.or(l, r)

      override def and(l: => A, r: => A): A = excludedMiddle0.and(l, r)

      override def Or: Associative[OrF[A]] = excludedMiddle0.Or

      override def And: Identity[AndF[A]] = excludedMiddle0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An `Involution` and an `Equal` for the same type in one value. The laws in
 * `InvolutionLaws`, in zio-prelude-experimental-laws, are stated for this type.
 * It is experimental, like `Involution`.
 */
trait InvolutionEqual[A] extends ExcludedMiddleEqual[A] with NoncontradictionEqual[A] with Involution[A]

/**
 * Companion of [[InvolutionEqual]], with its implicit derivation.
 */
object InvolutionEqual {

  /**
   * Combines the implicit `Involution[A]` and `Equal[A]` into an
   * `InvolutionEqual[A]` that delegates `complement`, `or`, `and`, `Or`, `And`
   * and equality to them. It does not forward `top` and `bottom`, so the result
   * uses the identities of `And` and `Or` even when the given instance
   * overrides them.
   */
  implicit def derive[A](implicit involution0: Involution[A], equal0: Equal[A]): InvolutionEqual[A] =
    new InvolutionEqual[A] {

      override def complement(a: => A): A = involution0.complement(a)

      override def or(l: => A, r: => A): A = involution0.or(l, r)

      override def and(l: => A, r: => A): A = involution0.and(l, r)

      override def Or: Identity[OrF[A]] = involution0.Or

      override def And: Identity[AndF[A]] = involution0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * A `Noncontradiction` and an `Equal` for the same type in one value. The laws
 * in `NoncontradictionLaws`, in zio-prelude-experimental-laws, are stated for
 * this type. It is experimental, like `Noncontradiction`.
 */
trait NoncontradictionEqual[A] extends AbsorptionEqual[A] with Noncontradiction[A]

/**
 * Companion of [[NoncontradictionEqual]], with its implicit derivation.
 */
object NoncontradictionEqual {

  /**
   * Combines the implicit `Noncontradiction[A]` and `Equal[A]` into a
   * `NoncontradictionEqual[A]` that delegates to them.
   */
  implicit def derive[A](implicit noncontradiction0: Noncontradiction[A], equal0: Equal[A]): NoncontradictionEqual[A] =
    new NoncontradictionEqual[A] {

      override def bottom: A = noncontradiction0.bottom

      override def complement(a: => A): A = noncontradiction0.complement(a)

      override def or(l: => A, r: => A): A = noncontradiction0.or(l, r)

      override def and(l: => A, r: => A): A = noncontradiction0.and(l, r)

      override def Or: Identity[OrF[A]] = noncontradiction0.Or

      override def And: Associative[AndF[A]] = noncontradiction0.And

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * A `DistributiveProd` and an `Equal` for the same type in one value. The laws
 * in `DistributiveProdLaws`, in zio-prelude-experimental-laws, are stated for
 * this type. It is experimental, like `DistributiveProd`.
 */
trait DistributiveProdEqual[A] extends DistributiveProd[A] with Equal[A]

/**
 * Companion of [[DistributiveProdEqual]], with its implicit derivation.
 */
object DistributiveProdEqual {

  /**
   * Combines the implicit `DistributiveProd[A]` and `Equal[A]` into a
   * `DistributiveProdEqual[A]` that delegates to them.
   */
  implicit def derive[A](implicit distributiveProd0: DistributiveProd[A], equal0: Equal[A]): DistributiveProdEqual[A] =
    new DistributiveProdEqual[A] {

      override def sum(l: => A, r: => A): A = distributiveProd0.sum(l, r)

      override def prod(l: => A, r: => A): A = distributiveProd0.prod(l, r)

      override def Sum: Associative[Sum[A]] = distributiveProd0.Sum

      override def Prod: Associative[Prod[A]] = distributiveProd0.Prod

      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}
