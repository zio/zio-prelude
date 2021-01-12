package zio.prelude

trait IdentityCompose[=>:[-_, +_]] extends AssociativeCompose[=>:] {
  def identity[A]: A =>: A

  def identityCompose[A, B](
    ab: A =>: B
  )(implicit eq: Equal[A =>: B]): Boolean = {
    val ab1 = compose(identity[B], ab)
    val ab2 = compose(ab, identity[A])

    eq.equal(ab1, ab) && eq.equal(ab2, ab)
  }
}
