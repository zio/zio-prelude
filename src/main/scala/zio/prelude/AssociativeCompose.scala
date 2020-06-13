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
  implicit val FunctionAssociativeCompose: AssociativeCompose[Function] =
    new AssociativeCompose[Function] {
      def compose[A, B, C](bc: B => C, ab: A => B): A => C =
        bc.compose(ab)
    }
}
