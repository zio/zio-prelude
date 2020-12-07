package zio.prelude

trait AssociativeCompose[:=>[-_, +_]] {
  def compose[A, B, C](bc: B :=> C, ab: A :=> B): A :=> C

  def associativeCompose[A, B, C, D](
    ab: A :=> B,
    bc: B :=> C,
    cd: C :=> D
  )(implicit eq: Equal[A :=> D]): Boolean = {
    val ad1 = compose(cd, compose(bc, ab))
    val ad2 = compose(compose(cd, bc), ab)

    eq.equal(ad1, ad2)
  }
}

object AssociativeCompose {
  implicit val FunctionIdentityCompose: IdentityCompose[Function] =
    new IdentityCompose[Function] {
      def identity[A]: A => A = (a: A) => a

      def compose[A, B, C](bc: B => C, ab: A => B): A => C =
        bc.compose(ab)
    }
}

trait AssociativeComposeSyntax {
  implicit class AssociativeComposeOps[:=>[-_, +_], A, B](private val ab: A :=> B) {

    /** Composes `A -> B` with `B -> C` to form `A -> C`. */
    def >>>[C](bc: B :=> C)(implicit ev: AssociativeCompose[:=>]): A :=> C =
      ev.compose(bc, ab)

    /** Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def <<<[Z](za: Z :=> A)(implicit ev: AssociativeCompose[:=>]): Z :=> B =
      ev.compose(ab, za)
  }
}
