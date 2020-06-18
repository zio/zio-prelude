package zio.prelude

trait IdentityCompose[:=>[-_, +_]] {
  def identity[A]: A :=> A

  def compose[A, B, C](bc: B :=> C, ab: A :=> B): A :=> C

  def identityCompose[A, B](
    ab: A :=> B
  )(implicit eq: Equal[A :=> B]): Boolean = {
    val ab1 = compose(identity[B], ab)
    val ab2 = compose(ab, identity[A])

    eq.equal(ab1, ab) && eq.equal(ab2, ab)
  }
}
object IdentityCompose {  
  implicit val FunctionIdentityCompose: IdentityCompose[Function] =
    new IdentityCompose[Function] {
      def identity[A]: A => A = (a: A) => a

      def compose[A, B, C](bc: B => C, ab: A => B): A => C =
        bc.compose(ab)
    }
}
