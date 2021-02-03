package zio.prelude

import zio.URIO

trait AssociativeCompose[=>:[-_, +_]] {
  def compose[A, B, C](bc: B =>: C, ab: A =>: B): A =>: C

  def associativeCompose[A, B, C, D](
    ab: A =>: B,
    bc: B =>: C,
    cd: C =>: D
  )(implicit eq: Equal[A =>: D]): Boolean = {
    val ad1 = compose(cd, compose(bc, ab))
    val ad2 = compose(compose(cd, bc), ab)

    eq.equal(ad1, ad2)
  }
}

object AssociativeCompose {

  implicit val FunctionIdentityCompose: IdentityCompose[Function] = new IdentityCompose[Function] {

    def identity[A]: A => A = scala.Predef.identity

    def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      bc.compose(ab)

  }

  implicit val URIOIdentityCompose: IdentityCompose[URIO] = new IdentityCompose[URIO] {
    def identity[A]: URIO[A, A] = URIO.environment

    def compose[A, B, C](bc: URIO[B, C], ab: URIO[A, B]): URIO[A, C] = ab.flatMap(bc.provide)
  }
}

trait AssociativeComposeSyntax {
  implicit class AssociativeComposeOps[A, B, =>:[-_, +_]](private val ab: A =>: B) {

    /** A symbolic alias for `andThen`. Composes `A -> B` with `B -> C` to form `A -> C`. */
    def >>>[C](implicit ev: AssociativeCompose[=>:]): (B =>: C) => (A =>: C) = { bc =>
      ev.compose(bc, ab)
    }

    /** Composes `A -> B` with `B -> C` to form `A -> C`. */
    def andThen[C](implicit ev: AssociativeCompose[=>:]): (B =>: C) => (A =>: C) = { bc =>
      ev.compose(bc, ab)
    }

    /** A symbolic alias for `compose`. Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def <<<[Z](implicit ev: AssociativeCompose[=>:]): (Z =>: A) => Z =>: B = { za =>
      ev.compose(ab, za)
    }

    /** Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def compose[Z](implicit ev: AssociativeCompose[=>:]): (Z =>: A) => Z =>: B = { za =>
      ev.compose(ab, za)
    }
  }
}
